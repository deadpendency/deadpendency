module Common.Effect.GitHub.CountPrivateInstalls.Backend.CountPrivateInstallsBackend
  ( githubCountPrivateInstalls,
  )
where

import Common.Model.Error.CommonError
import Common.Model.GitHub.Auth.GHInstallationAuth
import Data.Aeson
import Data.Text.Read qualified as Text
import Data.Vector qualified as V
import GitHub.REST.PageLinks qualified as GR
import Network.HTTP.Req
import Text.URI qualified as URI
import Text.URI.Lens qualified as URIL

githubCountPrivateInstalls :: GHInstallationAuth -> IO (Either CommonError Int)
githubCountPrivateInstalls gitAuth = runExceptT $ do
  installs <- getPages gitAuth 1
  let privateInstallsCount = V.length $ V.filter _isPrivate installs
  pure
    privateInstallsCount

-- https://docs.github.com/en/rest/reference/apps#installations
getPages :: GHInstallationAuth -> Int -> ExceptT CommonError IO (V.Vector Installation)
getPages gitAuth page = do
  let authToken = gitAuth ^. #_token
      url = https "api.github.com" /: "installation" /: "repositories"
      headers =
        header "Accept" "application/vnd.github+json"
          <> header "User-Agent" "deadpendency@deadpendency.com"
          <> header "Authorization" ("token " <> encodeUtf8 authToken)
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
      installations = responseBody @(JsonResponse Installations) responseInstallations
      -- last page does not have the header at all
      maybeNextPage = maybeLinkHeader >>= getNextPage

  case maybeNextPage of
    Nothing -> pure (installations ^. #_installations)
    Just nextPage -> do
      nextPageInt <- hoistEither $ first (\e -> GitHubResponseDecodeError $ "Can't decode next page: " <> nextPage <> " error: " <> e) (extractNextPage nextPage)
      let installs = installations ^. #_installations
      getPages gitAuth nextPageInt <&> \remainingPageInstalls -> installs V.++ remainingPageInstalls

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

newtype Installations = Installations
  { _installations :: V.Vector Installation
  }
  deriving stock (Generic)

newtype Installation = Installation
  { _isPrivate :: Bool
  }

instance FromJSON Installations where
  parseJSON = withObject "Installations" $ \v -> do
    installations <- v .: "repositories"
    pure $
      Installations installations

instance FromJSON Installation where
  parseJSON = withObject "Installation Details" $ \v -> do
    isPrivate <- v .: "private"
    pure $
      Installation isPrivate
