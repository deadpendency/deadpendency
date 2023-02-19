# Deadpendency Testing Setup

The testing setup is pretty straight forward. To be honest, probably not enough of the code is unit tested. However, this was a startup and I wanted to really focus on the best bang for buck testing so I could get the product out there.

## Technologies

* Unit Testing
* Property Based Testing (PBT)
* Smoke Testing

## Libraries

* [hspec](https://hspec.github.io/)
* [hedgehog](https://hedgehog.qa/)

## Overall Solution

Most of the code is broken down into effects which are structured as follows.

![/graphics/deadpendency-effect-internal-diagram](../graphics/deadpendency-effect-internal-structure.jpg)

Unit tests are written against the 'backends' or at lower levels. For example, the assess dependency backend breaks things down into a number of 'rules' and each of these rules have their own set of unit tests.

These tests will at times use PBT when appropriate.

## Effect Testing

The effect 'carriers' (or interpreters) are largely untested. This is because they tend to have boring plumbing sort of code. Reading in config which is then passed into functions in 'backends'.

Having said that, some effects have grown larger than they should be. Mostly because of the problem of trying to do concurrent IO not being easy to do via fused effects. Which meant lots of code was operating in IO and the effects could not be broken down in an easy to test way.

## Smoke Testing

There are a series of end to end tests that operate by:

1. Trigger a 'check run' to re-run via the GitHub API.
2. Wait for Deadpendency processing to complete and update the check run report.
3. Decode and confirm the report looks as expected.

These are run for various languages.

I believe that smoke tests are very valuable as something that can be run post deploy (and continuously) to gain a high degree of confidence that no major bugs are sneaking through. They also work as a nice form of integration test to ensure that the more targeted unit / integration tests are not missing something that is only exposed in the deployed application.

## Other Notable Tests / Decisions

### Tests Hit Actual External APIs

This is considered something of an anti-pattern but there are various tests that hit actual external APIs. Primarily this occurs in the tests which check the loading of dependency details from the various package registries.

Part of the reason this is considered an anti-pattern is because you can't run the tests if you don't have internet access. However, realistically these days I am either on wifi at home or tethered, so this isn't much of an issue. You are also testing more end to end, ensuring that your method of pulling the data from the API is working.

Still, this was done as a time saving hack.. Overall I would still not recommend it. At times these packages in the registries would change and I would lose that test case. It would definitely be better to replicate the test case in the test. I would only consider this if you want to cut corners on a startup.

### HTML Report Roundtrip Test

Probably based testing is used in some of the tests to not much effect to be honest. However, there is essentially 1 test for the HTML report generation using PBT roundtrip testing. Which is to say we generate a `Report` we convert it to HTML, then we convert it back to a `Report` and confirm nothing has changed.

This was extremely valuable as there are so many possible cases to test in the report. There are also all kinds of wacky ways the HTML parsing logic could fail. This parsing logic is required by the end to end smoke tests. Without the PBT roundtrip approach testing this code would have been very time consuming to maintain and error prone.
