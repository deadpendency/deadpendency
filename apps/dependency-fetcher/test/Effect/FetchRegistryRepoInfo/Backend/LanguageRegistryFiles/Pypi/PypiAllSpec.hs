{-# OPTIONS_GHC -fno-warn-orphans #-}

module Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Pypi.PypiAllSpec (spec) where

import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Pypi.Pypi
import Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Pypi.PypiIndex
import Streamly.Prelude qualified as S
import Test.Hspec

spec :: Spec
spec = parallel $
  context "when parsing real registry input" $ do
    xit "decodes the input correctly for some packages" $ do
      packageNames <- fetchPackageNames 100 4000
      result <- S.toList $ S.maxThreads 5 $ S.fromParallel $ S.mapM fetchDependencyPypi $ S.fromFoldable packageNames
      let resultLefts = fst $ partitionEithers result

      resultLefts `shouldBe` []
