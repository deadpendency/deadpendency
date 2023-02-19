{-# LANGUAGE DataKinds #-}

module FD.Handler.InstallationRepositoriesHandler
  ( installationRepositoriesHandler,
    InstallationRepositoriesRoute,
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventMessage (AppEventMessage (..))
import Control.Algebra (Has)
import Data.Vector.NonEmpty qualified as NV
import GitHub.Data.Webhooks.Events (InstallationRepoEventAction (..), InstallationRepositoriesEvent (..))
import GitHub.Data.Webhooks.Payload (whSimplRepoFullName)
import Servant (JSON, NoContent (..), Post, (:>))
import Servant.GitHub.Webhook (GitHubEvent, GitHubSignedReqBody, RepoWebhookEvent (..))

type InstallationRepositoriesRoute =
  GitHubEvent '[ 'WebhookInstallationRepositoriesEvent]
    :> GitHubSignedReqBody '[JSON] InstallationRepositoriesEvent
    :> Post '[JSON] NoContent

installationRepositoriesHandler ::
  ( Has AppEventEmit sig m
  ) =>
  RepoWebhookEvent ->
  ((), InstallationRepositoriesEvent) ->
  m NoContent
installationRepositoriesHandler _ (_, installationRepositoriesEvent) = do
  emitAppEventInfo (AppEventMessage "Handle installation repositories event")

  when
    (installationEventIsCreate installationRepositoriesEvent)
    do
      let reposAsText = installationEventToRepos installationRepositoriesEvent
      emitAppEventInfo (AppEventMessage ("Installed To Repos: " <> reposAsText))

  emitAppEventInfo (AppEventMessage "Finished installation repositories event")

  pure NoContent

installationEventIsCreate :: InstallationRepositoriesEvent -> Bool
installationEventIsCreate event =
  event ^. #evInstallationRepoAction == InstallationRepoAddedAction

installationEventToRepos :: InstallationRepositoriesEvent -> Text
installationEventToRepos event =
  case NV.fromVector (event ^. #evInstallationReposAdd) of
    Nothing -> error "unexpected no repos added with repo created event"
    Just nvRepos ->
      let repoNames = (nvRepos <&> whSimplRepoFullName)
       in foldl' (\acc fName -> acc <> ", " <> fName) (NV.head repoNames) (NV.tail repoNames)
