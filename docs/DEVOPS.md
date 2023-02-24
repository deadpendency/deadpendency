# Deadpendency 'Devops' Setup

For lack of a better term, this details how the application is built and tested, locally and in CI, then deployed.

There are too many small details to list here. To trace it through, step through the [CI steps](../.buildkite/pipeline.yml) and the underlying scripts.

## Technology Choices

### Google Cloud

[Why I chose google cloud](https://alistairb.dev/haskell-on-google-cloud-is-great/). Although, to be fair, `gogol` is currently in a much worse state than `amazonka` which has a regular maintainer.

### Stack

I think there are many pros and cons to using stack vs cabal. However, this is why I chose stack.

#### Easy Stackage Support

I think that easy usage of [Stackage](https://www.stackage.org/) is THE killer feature of stack.

* It is far easier to update dependencies. Just bump your resolver version and you are done. You know you will not get random incompatibilities, which is pretty huge.
* It is safer, as not only does stackage ensure package versions compile together, it also runs their tests to make sure they pass. With cabal, you only know that your dependency versions compile together.

#### Better UX

Stack just works like you would expect. I think cabal has lots of odd corners like `cabal configure`. Stuff like `--file-watch` is also very welcome.

#### Auto Updating Modules

For cabal you need to manually list every module in the cabal file, whereas stack ([hpack](https://hackage.haskell.org/package/hpack)) does this for you.

#### Why Not Nix?

I have tried out Nix a couple of times. It seems like it is very powerful but has a lot of sharp corners. My current assessment is it is not worth the complexity/niche tool burden for the benefits it brings, at least the case of Deadpendency. I already have quite good caching + reproducibility when building locally / in CI without much complexity via stack.

#### Why Not Cabal?

Aside from the benefits listed above, there is a lot to like about cabal. It apparently will  [get nice Stackage support](https://github.com/haskell/cabal/issues/7556). The project is also very healthy and under constant development.

Stack was looking relatively dead in terms of development, so I was looking to switch once Stackage support is in place. However, stack has been revived by Mike Pilgrem recently, so now it is less clear. I would still probably switch to cabal when possible. I think we are (or can be) at a point where we no longer need this split in the community and it would be a good thing.

#### Stackage Resolver

I have seen the argument that [using stackage nightly is preferrable](http://neilmitchell.blogspot.com/2015/12/whats-point-of-stackage-lts.html). There are some good arguments here.

I do think LTS does have its own advantages such as being slower to update, which means that breakage / bugs in new package versions are more likely to be fixed by the time they appear in LTS. Once a new GHC version appears in LTS, you know it is considered quite stable and has been well tested by the people on nightly. For a business, stability and minimising work are more important than being on the bleeding edge.

It is true that the LTS major version upgrades are challenging, but I think this will always be the case.

The other key reason is when you are on nightly, you will get stuck on the nightly when it moves to the bleeding edge GHC version, which you very likely cannot switch to at that point as will lack many many packages. For that period of time you are no longer getting any updates and will need to do a big upgrade eventually anyway by the time a switch is possible. Meanwhile LTS will continue to get nice small bumps.

## Auto Scripts Pattern

All the scripts live in the [auto](../auto/) directory. This was a pattern from a previous company I worked at. However, I think were I to do it again I would just go with Makefiles.

## Building / Testing locally

There are a few key points, but this is just using features from stack. The [stack.yaml](../stack.yaml) defines a multi package project and uses a custom stackage resolver.

## Building / Caching in CI

The CI platform in use is [buildkite](https://buildkite.com/), but those details are not particularly important. The basic method of caching in CI is to build and cache dependencies in Docker.

There are a few key points:

* Have `./root/.stack` as a persistent docker volume (mapped to a google cloud bucket). This means that dependencies can be compiled once and used in later CI steps.
* Use a [custom resolver](https://docs.haskellstack.org/en/stable/yaml_configuration/#resolver-or-snapshot). This means that dependencies / versions which are added manually and are not in the snapshot will get cached as global dependencies (unlike `extra-deps`). Thus they are compiled to `./root/.stack` and can easily be cached alongside the standard packages in the resolver.
* Use `stack test --only-dependencies` in a CI step before other steps. Thus the later CI steps can do something like `stack exec hlint` without needing to compile anything.

## Verification

There are a series of scripts in [auto/verify/haskell/](./auto/verify/haskell/) that can simply be run either locally or in CI and the script will do the right thing. Refer to the [readme for the full list](../README.md#verification).

### 'CI Build Check'

There is a small optimisation in the CI build that runs multiple steps at once. Deadpendency is not small and compiling the source Haskell on a CI agent takes about ~4 minutes when competing with other steps. Keeping everything separate means it builds:

* when running `stack test`
* when checking for ghc warnings `stack test --no-run-tests --pedantic`
* when running weeder `stack test --no-run-tests --ghc-options '-fwrite-ide-info' --exec weeder`
* when building the actual binaries to be deployed `stack build --copy-bins--ghc-options=-O2`

We can run the first 3 together via the [ci-build-check](../auto/verify/haskell/ci-build-check) to reduce the number of times we need to compile the Deadpendency source.

## Tools Deps

The basic method to using development tools (eg. linting) is to use them from the same stackage resolver as your code. This is done with [weeder](https://hackage.haskell.org/package/weeder) (detects dead code) which must use a version built against the same version of GHC as your code.

This is included in a `dev-deps` [package](../dev-deps/package.yaml) which is included via the stack.yaml. This means you can simply do `stack exec weeder` from the project root and it will compile and run the version from your stackage resolver. This package is not depended on via the application packages, so when you `stack build front-door` it is not compiled or bundled in.

### Separate Tools Deps Stack.yaml

The above method was used for all dev tools, however  eventually [ormolu](https://hackage.haskell.org/package/ormolu) added support for [cabal-the-libary](https://hackage.haskell.org/package/Cabal). This version conflicted with the version used in the `dependency-determiner` which also uses cabal-the-library.

An alternate method was used which is a separate [stack-dev-deps.yaml](../stack-dev-deps.yaml) to split the dev deps entirely. This also has the benefit that newer versions of `hlint` and `ormolu` can be used ie. by using the latest stackage nightly on a newer version of GHC than the application.

`weeder` must use the same GHC version as the application, so is still using the `dev-deps` package pattern.

## Overall Release Process

1. [Build a binary for each component](../auto/build-to-local).
2. [Embed the binaries in minimal Debian images](../support/app/Dockerfile)
3. [Push the images to google cloud](../gcloud/bin/release-app)
4. [Deploy a new blue / green version to non-live](../.buildkite/commands/deploy-app).
5. [Switch the live / non-live version](../.buildkite/commands/switch-common-front).
6. [Smoke test the new version](../.buildkite/commands/smoke-test-roll-back)
7. [Switch live / non-live again, if the smoke test fails](../.buildkite/commands/rollback-to-old-live)
