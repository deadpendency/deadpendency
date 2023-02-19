{-# LANGUAGE DataKinds #-}

module Common.Effect.GitHub.DetermineAccountPlan.Backend.DetermineAccountPlanBackend
  ( githubDetermineAccountPlan,
    MarketplaceResult (..),
  )
where

import Common.Model.Error.CommonError
import Common.Model.GitHub.Auth.GHSharedAppAuth
import Common.Model.Plan.Plan
import Data.Aeson
import Network.HTTP.Client qualified as HC
import Network.HTTP.Req
import Network.HTTP.Types.Status qualified as HC

githubDetermineAccountPlan :: GHSharedAppAuth -> Int -> IO (Either CommonError (Maybe MarketplaceResult))
githubDetermineAccountPlan appAuth ownerAccountId = do
  let authToken = appAuth ^. #_jwtToken
      url = https "api.github.com" /: "marketplace_listing" /: "accounts" /: show ownerAccountId
      headers =
        header "Accept" "application/vnd.github+json"
          <> header "User-Agent" "deadpendency@deadpendency.com"
          <> header "Authorization" ("Bearer " <> encodeUtf8 authToken)
  eitherExceptionOrA <-
    try @_ @HttpException $
      runReq defaultHttpConfig $
        responseBody
          <$> req
            GET
            url
            NoReqBody
            jsonResponse
            headers
  pure $
    case first (convertExceptionToError url) eitherExceptionOrA of
      Right a -> Right a
      Left Nothing -> Right Nothing
      Left (Just commonError) -> Left commonError

convertExceptionToError :: Url 'Https -> HttpException -> Maybe CommonError
convertExceptionToError url =
  \case
    VanillaHttpException (HC.HttpExceptionRequest _ (HC.StatusCodeException r _)) ->
      let statusCode = HC.statusCode $ HC.responseStatus r
       in if statusCode == 404
            then Nothing
            else Just $ GitHubInteractError $ show url <> " - " <> "Unexpected status code: " <> show statusCode
    unmatched -> Just $ GitHubInteractError $ show url <> " - " <> "Request exception: " <> show unmatched

data MarketplaceResult = MarketplaceResult
  { _pendingPlan :: Maybe Plan,
    _plan :: Plan
  }
  deriving stock (Generic)

instance FromJSON MarketplaceResult where
  parseJSON = withObject "Marketplace Result" $ \v -> do
    maybePendingPlan <-
      (v .:? "marketplace_pending_change") >>= \case
        Nothing -> pure Nothing
        Just pendingChange -> do
          pendingPlan <- pendingChange .: "plan"
          pendingPlanId <- pendingPlan .: "id"
          case planIdToPlan pendingPlanId of
            Just finalPendingPlan -> pure $ Just finalPendingPlan
            Nothing -> fail $ "Unexpected failure to parse pending plan: " <> show pendingPlanId

    marketplacePurchase <- v .: "marketplace_purchase"
    plan <- marketplacePurchase .: "plan"
    planId <- plan .: "id"
    finalPlan <-
      case planIdToPlan planId of
        Just finalPlan -> pure finalPlan
        Nothing -> fail $ "Unexpected failure to parse plan: " <> show planId

    pure $
      MarketplaceResult maybePendingPlan finalPlan
