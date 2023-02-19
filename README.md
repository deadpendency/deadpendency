# 🧟‍♀️ Deadpendency 🧟‍♂️

Check if my repo's dependencies are dead (projects)!

Check out the [website](https://deadpendency.com/) for more details on what the tool does.

Yet, RIP, Deadpendency is [dead](https://alistairb.dev/shutting-down-deadpendency/), shutdown and the code is open sourced. This repo just serves as an example of a relatively complex Haskell project deployed to the cloud.

## 📢 Overview

This documentation is focused on detailing the Haskell code and setup. In particular, the way the code is built, verified and deployed is quite nice. It is something I have iterated on for a number of years with a strong focus on simplicity, reproducibility and cacheability.

Code structure wise I think it is pretty good. This is my first large Haskell project and no doubt there are areas that can be simplified. This was also a startup, so there are corners that were cut and over time I did not focus on maintaining code quality as much, since I suspected the product would not work out. Still, overall I am quite happy with it and I have refactored quite a lot to achieve something that I found easy to work with and reason about.

Included is a bunch of google cloud configuration in the form of terraform files. This is my first time working with google cloud and terraform. I don't think it is bad, but I'm sure there is a bunch that could be improved. Note: All google cloud infrastructure is completely deleted at this point.

I've made a decent effort to document and make the code base approachable. However, I don't want to sink too much time into this and while developing it was just me and I did not need much documentation. So at a certain point you will need to walk through the code / scripts if you want to understand it deeply.

I am happy to explain or document certain things more if people have questions. Feel free to open issues.

## Running As A CLI Tool

Deadpendency was built to work only as a [GitHub app](https://docs.github.com/en/developers/apps/getting-started-with-apps/about-apps) running on [google cloud run](https://cloud.google.com/run). Even in its current form, no attempt was made to have it run locally. Part of this was timesaving, but also I personally am less keen on bespoke setups to run locally. I prefer to invest that time in better automated testing.

Nonetheless, it could certainly be converted into a CLI tool as all the logic required is there. The main difference would be a CLI interface would be required as well as a lot of refactoring to rip out all the google cloud dependencies.

## 🗒️ Documentation

* [Architecture](./docs/ARCHITECTURE.md)
* ['devops' pattern](./docs/DEVOPS.md)
* [Testing](./docs/TESTING.md)

## 💡 Technologies

### System

* [Google Cloud Platform (GCP)](https://cloud.google.com/)
* [Google Cloud Run](https://cloud.google.com/run)
* [Docker](https://www.docker.com/)
* [GitHub GraphQL API](https://docs.github.com/en/graphql)
* [Redis](https://redis.io/)
* [Terraform](https://www.terraform.io/)

### Code

* [Haskell](https://www.haskell.org/)
* [Stack](https://docs.haskellstack.org/en/stable/)

### Library

* [Relude](https://hackage.haskell.org/package/relude)
* [Generic Lens](https://hackage.haskell.org/package/generic-lens)
* [Fused Effects](https://hackage.haskell.org/package/fused-effects)
* [Servant](https://docs.servant.dev/en/stable/)
* [Morpheus GraphQL](https://morpheusgraphql.com/)
* [Gogol](https://hackage.haskell.org/package/gogol)
* [Streamly](https://streamly.composewell.com/)

And a bunch of other usual suspects in Haskell.

## 💻 Development

### Requirements

* Haskell Stack.
* Global GHC Install (currently needs 9.2.5) (eg. via [ghcup](https://www.haskell.org/ghcup/)).
* Docker
* [Haskell Lanauge Server](https://haskell-language-server.readthedocs.io/en/latest/) + Your Preferred Editor

### Building

```bash
auto/build
```

### Testing

```bash
auto/verify/haskell/test

# Or I typically just do
stack test
```

### Building / Testing With Docker

```bash
RUN_DOCKER=true auto/build
RUN_DOCKER=true auto/verify/haskell/test
```

### Building / Testing Single Packages

```bash
stack test dependency-determiner
```

### Format Code

```bash
# format modified / new files based on `git status`
auto/format-haskell

# format all haskell source files
auto/format-haskell ALL
```

### Verification

This will list the Haskell related checks, which is the focus of this doco. Refer to the other scripts in [auto/verify/*](./auto/verify/) to look at other checks that are run.

#### Run Full

```bash
auto/verify/all
```

#### Hlint

```bash
# hlint check modified / new files based on `git status`
auto/verify/haskell/hlint

# hlint check all files
auto/verify/haskell/hlint ALL
```

#### Ormolu

```bash
# format check modified / new files based on `git status`
auto/verify/haskell/ormolu

# format check all files
auto/verify/haskell/ormolu ALL
```

#### Weeder

Check for dead code.

```bash
auto/verify/haskell/weeder
```

#### GHC Warning Check

```bash
auto/verify/haskell/ghc-check
```

### Deploying New Versions

#### Project

Set up / update the google cloud project (roughly equivalent to an AWS Account)

```bash
terraform/project/plan

terraform/project/apply-update
```

#### Deploy [Blue / Green Version](https://en.wikipedia.org/wiki/Blue-green_deployment)

```bash
# BUILDKITE_COMMIT matches the docker tag pushed as part of ./gcloud/auto/release-app
APP_ENV=prod APP_LIVE=a BUILDKITE_COMMIT=1234 terraform/app/pipeline/plan

APP_ENV=prod APP_LIVE=a BUILDKITE_COMMIT=1234 terraform/app/pipeline/apply-update
```

#### Switch The Blue / Green Version

and deploy other common infrastructure which is not tied to a specific blue / green version.

```bash
APP_ENV=preprod APP_LIVE=a APP_LIVE_HOST=https://preprod-a-deadpendency-action-front-door-rnbiybubyq-uc.a.run.app terraform/app/common-front/plan

APP_ENV=preprod APP_LIVE=a APP_LIVE_HOST=https://preprod-a-deadpendency-action-front-door-rnbiybubyq-uc.a.run.app terraform/app/common-front/apply-update
```
