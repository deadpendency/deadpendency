module SR.Effect.FetchInstallationsCount.Backend.FetchInstallationsCountBackend
  ( githubFetchInstallationsCount,
  )
where

import Common.Model.Error.CommonError
import Common.Model.GitHub.Auth.GHSharedAppAuth
import Common.Model.Plan.Plan
import Data.Aeson
import Data.Text.Read qualified as Text
import Data.Vector qualified as V
import GitHub.REST.PageLinks qualified as GR
import Network.HTTP.Req
import Text.URI qualified as URI
import Text.URI.Lens qualified as URIL

githubFetchInstallationsCount :: GHSharedAppAuth -> Plan -> IO (Either CommonError Int)
githubFetchInstallationsCount gitAuth plan = runExceptT $ do
  installs <- getPages gitAuth plan 1
  let stillInstalledCount = V.length $ V.filter _isInstalled installs
  pure
    stillInstalledCount

-- https://docs.github.com/en/rest/reference/apps#list-accounts-for-a-plan
getPages :: GHSharedAppAuth -> Plan -> Int -> ExceptT CommonError IO (V.Vector Installation)
getPages gitAuth plan page = do
  let authToken = gitAuth ^. #_jwtToken
      url = https "api.github.com" /: "marketplace_listing" /: "plans" /: show (planToPlanId plan) /: "accounts"
      headers =
        header "Accept" "application/vnd.github+json"
          <> header "User-Agent" "deadpendency@deadpendency.com"
          <> header "Authorization" ("Bearer " <> encodeUtf8 authToken)
          <> queryParam "per_page" (Just (100 :: Int))
          <> queryParam "page" (Just page)
  responseInstallations <-
    ExceptT $
      firstF (GitHubInteractError . show) $
        try @_ @HttpException $
          runReq defaultHttpConfig $
            req
              GET
              url
              NoReqBody
              jsonResponse
              headers
  let maybeLinkHeader = decodeUtf8 <$> responseHeader responseInstallations "link"
      installations = responseBody @(JsonResponse (V.Vector Installation)) responseInstallations
      -- last page does not have the header at all
      maybeNextPage = maybeLinkHeader >>= getNextPage

  case maybeNextPage of
    Nothing -> pure installations
    Just nextPage -> do
      nextPageInt <- hoistEither $ first (\e -> GitHubResponseDecodeError $ "Can't decode next page: " <> nextPage <> " error: " <> e) (extractNextPage nextPage)
      getPages gitAuth plan nextPageInt <&> \remainingPageInstalls -> installations V.++ remainingPageInstalls

-- link: <https://api.github.com/installation/repositories?per_page=2&page=2>; rel="next", <https://api.github.com/installation/repositories?per_page=2&page=6>; rel="last"
getNextPage :: Text -> Maybe Text
getNextPage = GR.pageNext . GR.parsePageLinks

extractNextPage :: Text -> Either Text Int
extractNextPage input = do
  uri <- first (pack . show) $ URI.mkURI input
  queryParamPageKey <- first (pack . show) $ URI.mkQueryKey "page"
  let maybeUri = uri ^? (URIL.uriQuery . URIL.queryParam queryParamPageKey)
  urlAsText <- maybeToRight "Page query param missing" $ URI.unRText <$> maybeUri
  bimap pack fst $ Text.decimal urlAsText

newtype Installation = Installation
  { _isInstalled :: Bool
  }

instance FromJSON Installation where
  parseJSON = withObject "Installation Details" $ \v -> do
    marketplacePurchase <- v .: "marketplace_purchase"
    isInstalled <- marketplacePurchase .: "is_installed"
    pure $
      Installation isInstalled
