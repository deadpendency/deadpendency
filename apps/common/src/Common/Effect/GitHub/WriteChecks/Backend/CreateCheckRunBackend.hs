{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-deriving-strategies #-}

module Common.Effect.GitHub.WriteChecks.Backend.CreateCheckRunBackend
  ( githubCreateCheckRun,
  )
where

import Common.Effect.GitHub.WriteChecks.Model.CheckRunCreateRequest qualified as A
import Common.Effect.GitHub.WriteChecks.Model.CheckRunCreateResult
import Common.GitHub.GraphQLCommon
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Error.CommonError
import Common.Model.Git.GitSha
import Common.Model.GitHub.Auth.GHInstallationAuth
import Common.Model.GitHub.Checks.CheckRun qualified as A
import Common.Model.GitHub.Checks.CheckRunStatus qualified as A
import Common.Model.GitHub.GHNodeId
import Data.Morpheus.Client
import Data.Vector qualified as V

declareGlobalTypesByName "../resource/minimal-github.graphql" ["CreateCheckRunInput", "RequestableCheckStatusState", "CheckStatusState"]

declareLocalTypesInline
  "../resource/minimal-github.graphql"
  [raw|
    mutation CreateCheckRun ($checkRunInput: CreateCheckRunInput!) {
      createCheckRun(input: $checkRunInput) {
        checkRun {
          checkRunId: id
          checkRunName: name
          checkRunStatus: status
          repository {
            repositoryIdOut: id
            languages(first: 4, orderBy: {field: SIZE, direction: DESC}) {
              nodes {
                languageName: name
              }
            }
          }
          checkSuite {
            commit {
              oid
            }
          }
        }
      }
    }
  |]

githubCreateCheckRun :: GHInstallationAuth -> A.CheckRunCreateRequest -> IO (Either CommonError CheckRunCreateResult)
githubCreateCheckRun gitAuth request' = do
  eitherRawCheckRun <- mutCreateCheckRun authToken args
  pure $ asCommonMissingError eitherRawCheckRun >>= mapRunOutput
  where
    args = produceRunInput request'
    authToken = gitAuth ^. #_token

mutCreateCheckRun :: Text -> CreateCheckRunArgs -> IO (Either (FetchError CreateCheckRun) CreateCheckRun)
mutCreateCheckRun = fetch . executeGraphQL'

produceRunInput :: A.CheckRunCreateRequest -> CreateCheckRunArgs
produceRunInput request' =
  CreateCheckRunArgs
    { checkRunInput =
        CreateCheckRunInput
          { headSha = headSha',
            name = name',
            repositoryId = repositoryId',
            status = RequestableCheckStatusStateIN_PROGRESS
          }
    }
  where
    headSha' = GitObjectID $ request' ^. (#_headSha . #_ntText)
    name' = request' ^. #_name
    repositoryId' = ID $ request' ^. (#_repoNodeId . #_ntText)

mapRunOutput :: CreateCheckRun -> Either CommonError CheckRunCreateResult
mapRunOutput ccr = first GitHubResponseDecodeError $
  do
    checkRunPayload' <- maybeToRight "CheckRunPayloadMissing" $ createCheckRun ccr
    checkRun' <- maybeToRight "CheckRunMissing" $ checkRun checkRunPayload'
    let repository' = repository checkRun'
        name' = checkRunName checkRun'
        commitSha' = (_gitObjectIDText . oid . commit . checkSuite) checkRun'
        gitSha' = GitSha commitSha'
    let repositoryId' = GHNodeId $ (unpackID . repositoryIdOut) repository'
        checkRunId' = GHNodeId $ (unpackID . checkRunId) checkRun'
        status' = toAppStatus $ checkRunStatus checkRun'
        outputCheckRun =
          A.CheckRun
            { A._nodeId = checkRunId',
              A._headSha = gitSha',
              A._name = name',
              A._repoNodeId = repositoryId',
              A._checkRunStatus = status',
              A._checkRunConclusion = Nothing
            }
    let languages = repository' ^.. (#languages . _Just . #nodes . _Just . to catMaybes . folded . #languageName . to toProgrammingLanguage)
        vecLanguages = V.take 2 $ noDupV $ V.fromList languages
    pure
      CheckRunCreateResult
        { _checkRun = outputCheckRun,
          _repoProgrammingLanguages = vecLanguages
        }

toAppStatus :: CheckStatusState -> A.CheckRunStatus
toAppStatus CheckStatusStateQUEUED = A.CheckRunStatusQueued
toAppStatus CheckStatusStateREQUESTED = A.CheckRunStatusRequested
toAppStatus CheckStatusStateIN_PROGRESS = A.CheckRunStatusInProgress
toAppStatus CheckStatusStateCOMPLETED = A.CheckRunStatusCompleted

toProgrammingLanguage :: Text -> ProgrammingLanguage
toProgrammingLanguage =
  \case
    "JavaScript" -> JavaScript
    "TypeScript" -> TypeScript
    -- some languages implies JS
    "HTML" -> JavaScript
    "CSS" -> JavaScript
    "SCSS" -> JavaScript
    "Less" -> JavaScript
    "CoffeeScript" -> JavaScript
    "Elm" -> JavaScript
    -- others
    "Python" -> Python
    "PHP" -> Php
    "Ruby" -> Ruby
    "Haskell" -> Haskell
    "Rust" -> Rust
    "C#" -> CSharpNet
    "Visual Basic" -> VisualBasicNet
    "Java" -> Java
    "Kotlin" -> Kotlin
    "Scala" -> Scala
    "Go" -> Golang
    other -> UnsupportedLanguage other
