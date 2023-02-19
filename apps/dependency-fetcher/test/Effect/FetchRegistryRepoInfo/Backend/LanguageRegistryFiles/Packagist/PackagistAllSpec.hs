{-# OPTIONS_GHC -fno-warn-orphans #-}

module Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Packagist.PackagistAllSpec (spec) where

import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.Registry.DependencyRegistryInfo
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Packagist.Packagist
import DF.Effect.FetchRegistryRepoInfo.Backend.Model.FetchDependencyRegistryError
import Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Packagist.PackagistIndex
import Streamly.Prelude qualified as S
import Test.Hspec

spec :: Spec
spec = parallel $
  context "when parsing real registry input" $ do
    xit "decodes the input correctly for some packages" $ do
      packageNames <- fetchPackageNames 100 2000
      result <- S.toList $ S.maxThreads 5 $ S.fromParallel $ S.mapM failOnNothingFetch $ S.fromFoldable packageNames
      let resultLefts = fst $ partitionEithers result

      resultLefts `shouldBe` []

failOnNothingFetch :: DependencyName -> IO (Either FetchDependencyRegistryError DependencyRegistryInfo)
failOnNothingFetch name = do
  result <- fetchDependencyPackagist name
  case result of
    Right (Just a) -> pure $ Right a
    Right Nothing -> error $ "failed to fetch: " <> name ^. #_ntText
    Left b -> pure $ Left b
