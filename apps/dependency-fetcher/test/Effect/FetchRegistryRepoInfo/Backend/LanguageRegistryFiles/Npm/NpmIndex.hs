module Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Npm.NpmIndex
  ( fetchPackageNames,
  )
where

import Common.Model.Dependency.DependencyName
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Internal
import Data.Aeson
import Data.Vector qualified as V
import Network.HTTP.Req

fetchPackageNames :: Int -> Int -> IO (V.Vector DependencyName)
fetchPackageNames count skip = do
  let queryString =
        "limit"
          =: show @Text count
          <> "skip"
            =: show @Text skip
      url = https "replicate.npmjs.com" /: "_all_docs"
  eitherMaybeNpmIndex <- fetchJSON' @NpmIndex queryString url
  pure $
    case eitherMaybeNpmIndex of
      Right (Just npmIndex) -> npmIndex ^. #_result
      Right Nothing -> error "got no results"
      Left a -> error $ "boom: " <> show a

newtype NpmIndex = NpmIndex
  { _result :: V.Vector DependencyName
  }
  deriving stock (Show, Generic)

{-
{
  "total_rows": 1459150,
  "offset": 0,
  "rows": [
    {
      "id": "-",
      "key": "-",
      "value": {
        "rev": "1-e340e64b090ea8c6b4e14edc0460c751"
        }
    },
    {
      "id": "-keyboardevent",
      "key": "-keyboardevent",
      "value": {
        "rev": "1-0fdff278618c4f8c1026889f1b525483"
      }
    }
}
-}

instance FromJSON NpmIndex where
  parseJSON =
    withObject "NpmIndex" $ \v -> do
      rows <- v .: "rows"
      names <- traverse (.: "id") rows

      pure $
        NpmIndex $
          DependencyName <$> names
