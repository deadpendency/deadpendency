#!/bin/bash
set -Eexuo pipefail

# get jwt
auto/app-auth-token

jq '.jsonPayload.additional | .[] | .dependencyIdentifier.contents' big.json | xargs -I {} -n 1 curl -O https://registry.npmjs.org/{}
jq '.jsonPayload.additional | .[] | .dependencyIdentifier.contents' big.json | xargs -I {} -n 1 echo curl -H "Accept: application/json" https://registry.npmjs.org/{} -o "{}.json"

# auth as app installation
curl -i -X POST \
-H "Authorization: Bearer AUTH_TOKEN" \
-H "Accept: application/vnd.github+json" \
https://api.github.com/app/installations/11238978/access_tokens


curl \
  -H "Authorization: Bearer AUTH_TOKEN" \
  -H "Accept: application/vnd.github+json" \
  https://api.github.com/app/hook/deliveries

curl -i \
-H "Authorization: Bearer AUTH_TOKEN" \
-H "Accept: application/vnd.github+json" \
https://api.github.com/marketplace_listing/accounts/6412433

# https://api.github.com/app/installations/11324639/access_tokens
# preprod test repo

curl \
  -H "Authorization: Bearer AUTH_TOKEN" \
  -H "Accept: application/vnd.github+json" \
  https://api.github.com/marketplace_listing/plans/5541/accounts?per_page=100&page1

query {
  repository(owner:\"deadpendency\", name: \"deadpendency-test-repo\") {
    dependencyGraphManifests(first:2) {
      edges {
        node {
          blobPath
          filename
          dependencies(first: 2) {
            nodes {
              packageName
              packageManager
            }
          }
        }
      }
    }
  }
}

query {
  repository(owner: "haskell", name: "cabal") {
    isArchived
    isFork
    defaultBranchRef {
      target {
        ... on Commit {
          history(first: 2, since: "2018-06-22T00:00:00Z") {
            edges {
              node {
                pushedDate
                committedDate
                author {
                  authorEmail: email
                }
              }
            }
          }
        }
      }
    }
  }
}

query {
  repository(owner:"mokacoding", name: "symlinks") {
		object(expression: "65e46aa9c2fd85d93d97fa9acf2c1365d7157f42:bar") {
        __typename
        ... on Tree {
          entries {
            name
            type
            extension
            isGenerated
            mode
            submodule {
              branch
              subprojectCommitOid
            }
            object {
              ... on Blob {
                isTruncated
                commitResourcePath
                oid
                byteSize
                text
              }
            }
            oid
          }
        }
      }
    }
  }

# to run quickly
query {
  repository(owner:"deadpendency", name: "smoke-java") {
    id
		object(oid: "28fed143ea79dab8c452c2dbacedd8b73c2b41c2")
      {
        ... on Commit {
        checkSuites(first: 10)
        { edges
          {
            node {
              app {
                name
              }
            	checkSuiteId: id
              checkRuns(first: 10) {
                edges {
                  node {
                    checkRunId: id
                    name
                    startedAt
                    completedAt
                    status
                    conclusion
                    databaseId
                    externalId
                  }
                }
              }
          	}
          }
        }
      }
    }
  }
}

# fetch app
curl -i -H "Authorization: Bearer AUTH_TOKEN" -H "Accept: application/vnd.github+json" https://api.github.com/app

# installations accessible to token

curl -H "Authorization: Bearer AUTH_TOKEN" -H "Accept: application/vnd.github+json" https://api.github.com/app/installations | jq .[0]

curl -H "Authorization: token INSTALLATION_TOKEN" -H "Accept: application/vnd.github+json" https://api.github.com/installation/repositories

curl -v \
  -H "Authorization: token INSTALLATION_TOKEN" \
  -H "Accept: application/vnd.github+json" \
  https://api.github.com/installation/repositories?per_page=2&page=6

# see if account has a plan for the authenticated github app
curl -i -H "Authorization: Bearer AUTH_TOKEN" -H "Accept: application/vnd.github+json"  https://api.github.com/marketplace_listing/stubbed/accounts/437253

# search

curl -i -v -H "Authorization: Bearer AUTH_TOKEN" https://api.github.com/search/code?q=extension:gemspec+repo:rubocop-hq/rubocop

mutation {
  createCheckRun(input: {headSha: "c82bd71ab263062736c22a309fc140322b0e376f", name: "GraphQL Rocks!" }) {
    clientMutationId
  }
}
curl -H "Authorization: token INSTALLATION_TOKEN" -X POST -d " \
 { \
   \"query\": \"query { viewer { login }}\" \
 } \
" https://api.github.com/graphql

curl -H "Authorization: token INSTALLATION_TOKEN" \
 -H "Accept: application/vnd.github.hawkgirl-preview+json" \
 -X POST \
 -d "@./support/app-auth-token/get-dependencies.json" \
 https://api.github.com/graphql | jq

# https://github.com/deadpendency/deadpendency-test-repo/runs/2162110687

curl -H "Authorization: token INSTALLATION_TOKEN" \
 -H "Accept: application/vnd.github+json" \
 -X POST \
 -d "@./support/app-auth-token/create-check-run.json" \
 https://api.github.com/graphql | jq

curl -H "Authorization: token INSTALLATION_TOKEN" \
 -H "Accept: application/vnd.github+json" \
 -X POST \
 -d "@./support/app-auth-token/get-check-run.json" \
 https://api.github.com/graphql | jq

curl -H "Authorization: token INSTALLATION_TOKEN" \
 -H "Accept: application/vnd.github+json" \
 -X POST \
 -d "@./support/app-auth-token/rerequest-check-suite.json" \
 https://api.github.com/graphql | jq
