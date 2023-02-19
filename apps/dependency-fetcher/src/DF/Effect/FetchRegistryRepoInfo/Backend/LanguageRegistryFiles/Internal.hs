{-# LANGUAGE DataKinds #-}

module DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Internal
  ( fetchJSON,
    fetchJSON',
    fetchUrl,
    fetchUrl',
    fetchUrlRetry404,
    fetchJSONRetry404,
    getLatestReleaseKey,
    selectFinalRepo,
    stripVPrefix,
  )
where

import Common.Model.Git.QualifiedRepo
import Common.Model.Git.Repo
import Common.Parsing.Megaparsec
import Control.Concurrent (threadDelay)
import DF.Effect.FetchRegistryRepoInfo.Backend.Model.FetchDependencyRegistryError
import Data.Aeson
import Data.Text qualified as Text
import Data.Vector qualified as V
import Data.Versions (prettySemVer, semver)
import Network.HTTP.Client qualified as HC
import Network.HTTP.Req
import Network.HTTP.Types.Status qualified as HC
import Text.Megaparsec.Char qualified as M

fetchJSON :: (FromJSON a) => Url 'Https -> IO (Either FetchDependencyRegistryError (Maybe a))
fetchJSON = fetchInternal jsonResponse mempty

fetchJSON' :: (FromJSON a) => Network.HTTP.Req.Option 'Https -> Url 'Https -> IO (Either FetchDependencyRegistryError (Maybe a))
fetchJSON' = fetchInternal jsonResponse

fetchUrl :: Url 'Https -> IO (Either FetchDependencyRegistryError (Maybe ByteString))
fetchUrl = fetchInternal bsResponse mempty

fetchUrl' :: Url 'Https -> IO (Either FetchDependencyRegistryError (Maybe LByteString))
fetchUrl' = fetchInternal lbsResponse mempty

fetchUrlRetry404 :: Int -> Url 'Https -> IO (Either FetchDependencyRegistryError (Maybe ByteString))
fetchUrlRetry404 = fetchRetry404Internal bsResponse

fetchJSONRetry404 :: (FromJSON a) => Int -> Url 'Https -> IO (Either FetchDependencyRegistryError (Maybe a))
fetchJSONRetry404 = fetchRetry404Internal jsonResponse

fetchRetry404Internal :: (HttpResponse a) => Proxy a -> Int -> Url 'Https -> IO (Either FetchDependencyRegistryError (Maybe (HttpResponseBody a)))
fetchRetry404Internal responseType retryCount url
  | retryCount == 0 = fetchInternal responseType mempty url
  | otherwise = do
      result <- fetchInternal responseType mempty url
      case result of
        (Right (Just _)) -> pure result
        (Left _) -> pure result
        (Right Nothing) -> threadDelay 1000000 *> fetchRetry404Internal responseType (retryCount - 1) url

fetchInternal :: (HttpResponse a) => Proxy a -> Network.HTTP.Req.Option 'Https -> Url 'Https -> IO (Either FetchDependencyRegistryError (Maybe (HttpResponseBody a)))
fetchInternal responseType additionalOptions url = do
  let headers =
        additionalOptions
          <> header "Accept" "application/json"
          <> header "User-Agent" "deadpendency@deadpendency.com"
  eitherExceptionOrA <-
    try @_ @HttpException $
      runReq httpConfig $
        responseBody
          <$> req
            GET
            url
            NoReqBody
            responseType
            headers
  pure $
    case eitherExceptionOrA of
      Right a -> Right $ Just a
      Left e ->
        case convertExceptionToError url e of
          Just error' -> Left error'
          _ -> Right Nothing -- 404 is not considered exceptional, so in this case we return nothing

convertExceptionToError :: Url 'Https -> HttpException -> Maybe FetchDependencyRegistryError
convertExceptionToError url =
  \case
    JsonHttpException failure -> Just $ FDRFailureToParseResult (show url <> " - " <> pack failure)
    VanillaHttpException (HC.HttpExceptionRequest _ (HC.StatusCodeException r _)) ->
      let statusCode = HC.statusCode $ HC.responseStatus r
       in if statusCode == 404 || statusCode == 410
            then Nothing
            else Just $ FDRRegistryFetchExceptional $ show url <> " - " <> "Unexpected status code: " <> show statusCode
    -- packagist redirects 3 times when it loads a search page when the package can't be found response. We limit redirects and return nothing in this case.
    VanillaHttpException (HC.HttpExceptionRequest _ (HC.TooManyRedirects _)) -> Nothing
    unmatched -> Just $ FDRRegistryFetchExceptional $ show url <> " - " <> "Request exception: " <> show unmatched

httpConfig :: HttpConfig
httpConfig =
  defaultHttpConfig
    { httpConfigRedirectCount = 2
    }

-- versions that don't convert to semver should be dev versions eg. `dev-master` and thus are ignored
getLatestReleaseKey :: V.Vector Text -> Maybe Text
getLatestReleaseKey = fmap prettySemVer . safeMaximumV . snd . V.partitionWith semver

-- source repos if they exist will be used, but other project urls will only be considered a repo if they parse to the more specific QualifiedRepo type
selectFinalRepo :: V.Vector (Maybe Repo) -> V.Vector (Maybe QualifiedRepo) -> Maybe Repo
selectFinalRepo sourceRepos projectUrls =
  let maybeSourceRepo = safeHeadV $ concatMaybeV sourceRepos
   in case maybeSourceRepo of
        mSource@(Just _) -> mSource
        Nothing -> RepoQR <$> safeHeadV (concatMaybeV projectUrls)

stripVPrefix :: Text -> Text
stripVPrefix input
  | mParseMaybe parseVPrefix input == Just () = Text.tail input
  | otherwise = input

parseVPrefix :: MParser ()
parseVPrefix = do
  M.char 'v' *> M.digitChar
  pure ()
