{-# LANGUAGE DataKinds #-}

module FD.Handler.InstallationHandler
  ( installationHandler,
    InstallationRoute,
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventMessage (AppEventMessage (..))
import Control.Algebra (Has)
import GitHub.Data.Webhooks.Events (InstallationEvent (..), InstallationEventAction (..))
import Servant (JSON, NoContent (..), Post, (:>))
import Servant.GitHub.Webhook (GitHubEvent, GitHubSignedReqBody, RepoWebhookEvent (..))

type InstallationRoute =
  GitHubEvent '[ 'WebhookInstallationEvent]
    :> GitHubSignedReqBody '[JSON] InstallationEvent
    :> Post '[JSON] NoContent

installationHandler ::
  ( Has AppEventEmit sig m
  ) =>
  RepoWebhookEvent ->
  ((), InstallationEvent) ->
  m NoContent
installationHandler _ (_, installationEvent) = do
  let accountId = installationEvent ^. (#evInstallationSender . #whUserLogin)
  emitAppEventInfo $ AppEventMessage $ "Handle installation event: " <> accountId

  if
      | installationEventIsCreate installationEvent -> emitAppEventInfo (AppEventMessage $ "install-event - added installed: " <> accountId)
      | installationEventIsUnSuspend installationEvent -> emitAppEventInfo (AppEventMessage $ "install-event - added unsuspended: " <> accountId)
      | installationEventIsDelete installationEvent -> emitAppEventInfo (AppEventMessage $ "install-event - removed - uninstalled: " <> accountId)
      | installationEventIsSuspend installationEvent -> emitAppEventInfo (AppEventMessage $ "install-event - removed - suspended: " <> accountId)
      | otherwise -> pure ()

  emitAppEventInfo (AppEventMessage "Finished installation event")

  pure NoContent

installationEventIsCreate :: InstallationEvent -> Bool
installationEventIsCreate event =
  event ^. #evInstallationAction == InstallationCreatedAction

installationEventIsDelete :: InstallationEvent -> Bool
installationEventIsDelete event =
  event ^. #evInstallationAction == InstallationDeletedAction

installationEventIsSuspend :: InstallationEvent -> Bool
installationEventIsSuspend event =
  event ^. #evInstallationAction == InstallationSuspendAction

installationEventIsUnSuspend :: InstallationEvent -> Bool
installationEventIsUnSuspend event =
  event ^. #evInstallationAction == InstallationUnsuspendAction
