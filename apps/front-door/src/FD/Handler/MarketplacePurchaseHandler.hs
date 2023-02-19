{-# LANGUAGE DataKinds #-}

module FD.Handler.MarketplacePurchaseHandler
  ( marketplacePurchaseHandler,
    MarketplacePurchaseRoute,
  )
where

import Common.Aeson.Aeson
import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventAdditional (AppEventAdditional (AppEventAdditional))
import Common.Effect.AppEventEmit.Model.AppEventMessage (AppEventMessage (..))
import Common.Effect.MetricEmit.MetricEmit
import Common.Effect.MetricEmit.Model.MetricEvent
import Common.Model.Plan.Plan (planIdToPlan)
import Control.Algebra (Has)
import Data.Aeson
import GitHub.Data.Webhooks.Events (MarketplacePurchaseEvent (..), MarketplacePurchaseEventAction (..))
import Servant (JSON, NoContent (..), Post, (:>))
import Servant.GitHub.Webhook (GitHubEvent, GitHubSignedReqBody, RepoWebhookEvent (..))

type MarketplacePurchaseRoute =
  GitHubEvent '[ 'WebhookMarketplacePurchaseEvent]
    :> GitHubSignedReqBody '[JSON] MarketplacePurchaseEvent
    :> Post '[JSON] NoContent

marketplacePurchaseHandler ::
  ( Has AppEventEmit sig m,
    Has MetricEmit sig m
  ) =>
  RepoWebhookEvent ->
  ((), MarketplacePurchaseEvent) ->
  m NoContent
marketplacePurchaseHandler _ (_, marketplacePurchaseEvent) = do
  let purchaseEvent = getAppPurchase marketplacePurchaseEvent
  emitAppEventInfoA (AppEventMessage "Handle marketplace purchase event") (AppEventAdditional purchaseEvent)
  let eventAction = marketplacePurchaseEvent ^. #evMarketplacePurchaseAction
      planId = marketplacePurchaseEvent ^. (#evMarketplacePurchaseNew . #whMarketplacePurchasePlan . #whMarketplacePlanId)
      maybePlan = planIdToPlan planId
      login = marketplacePurchaseEvent ^. (#evMarketplacePurchaseNew . #whMarketplacePurchaseAccount . #whMarketplaceAccountLogin)

  case (maybePlan, eventAction) of
    (Nothing, _) -> emitAppEventErrorA (AppEventMessage "Unknown plan id") (AppEventAdditional planId)
    (Just plan, MarketplacePurchasePurchasedAction) -> metricEmit $ AppInstalledMetricEvent plan login
    (Just plan, MarketplacePurchaseCancelledAction) -> metricEmit $ AppCancelledMetricEvent plan login
    _ -> pure ()

  pure NoContent

getAppPurchase :: MarketplacePurchaseEvent -> AppPurchaseEvent
getAppPurchase marketplacePurchaseEvent =
  let login = marketplacePurchaseEvent ^. (#evMarketplacePurchaseNew . #whMarketplacePurchaseAccount . #whMarketplaceAccountLogin)
      accountId = marketplacePurchaseEvent ^. (#evMarketplacePurchaseNew . #whMarketplacePurchaseAccount . #whMarketplaceAccountId)
      event = show $ marketplacePurchaseEvent ^. #evMarketplacePurchaseAction
      plan = marketplacePurchaseEvent ^. (#evMarketplacePurchaseNew . #whMarketplacePurchasePlan . #whMarketplacePlanName)
   in AppPurchaseEvent
        { _login = login,
          _accountId = accountId,
          _event = event,
          _plan = plan
        }

data AppPurchaseEvent = AppPurchaseEvent
  { _login :: Text,
    _accountId :: Int,
    _event :: Text,
    _plan :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON AppPurchaseEvent where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions
