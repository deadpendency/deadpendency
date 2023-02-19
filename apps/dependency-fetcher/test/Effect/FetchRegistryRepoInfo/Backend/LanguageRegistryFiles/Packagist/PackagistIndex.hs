module Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Packagist.PackagistIndex
  ( fetchPackageNames,
  )
where

import Common.Model.Dependency.DependencyName
import Common.Parsing.Megaparsec
import Common.Parsing.NameParsing
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Internal
import Data.Aeson
import Data.Vector qualified as V
import Network.HTTP.Req

fetchPackageNames :: Int -> Int -> IO (V.Vector DependencyName)
fetchPackageNames count skip = do
  let url = https "packagist.org" /: "packages" /: "list.json"
  eitherMaybePackagistIndex <- fetchJSON @PackagistIndex url
  pure $
    case eitherMaybePackagistIndex of
      Right (Just packagistIndex) -> V.take count $ V.drop skip $ packagistIndex ^. #_result
      Right Nothing -> error "got no results"
      Left a -> error $ "boom: " <> show a

newtype PackagistIndex = PackagistIndex
  { _result :: V.Vector DependencyName
  }
  deriving stock (Show, Generic)

{-
{
  "packageNames": [
    "[vendor]/[package]",
    ...
  ]
}
-}

instance FromJSON PackagistIndex where
  parseJSON =
    withObject "PackagistIndex" $ \v -> do
      packages <- v .: "packageNames"
      let deps =
            case traverse (mParseMaybe parserDependencyName) packages of
              Nothing -> error "Unexpected name"
              Just depBits -> depBits

      pure $
        PackagistIndex deps
