{-# OPTIONS_GHC -fno-warn-orphans #-}

module Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Maven.MavenDetermineLatestReleaseSpec (spec) where

import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Maven.MavenDetermineLatestRelease
import Data.Vector.NonEmpty qualified as NV
import Test.Hspec

spec :: Spec
spec = parallel $
  context "determining latest release" $ do
    it "happy day" $ do
      let input =
            NV.unsafeFromList ["0.0.1", "1.0.0", "1.2.0", "1.0.1"]

          result = determineLatestRelease input

          expected = Just "1.2.0"

      result `shouldBe` expected

    it "identifies .Final releases as valid" $ do
      let input =
            NV.unsafeFromList ["5.5.0.Beta1", "5.5.0.CR1", "5.5.0.Final", "5.5.2.Final", "6.0.0.Alpha2"]

          result = determineLatestRelease input

          expected = Just "5.5.2.Final"

      result `shouldBe` expected

    it "identifies .RELEASE releases as valid" $ do
      let input =
            NV.unsafeFromList ["5.2.7", "5.2.8.RELEASE", "5.2.9.RELEASE"]

          result = determineLatestRelease input

          expected = Just "5.2.9.RELEASE"

      result `shouldBe` expected

    it "non .RELEASE, but later can override" $ do
      let input =
            NV.unsafeFromList ["5.2.8.RELEASE", "5.2.9.RELEASE", "5.3.2"]

          result = determineLatestRelease input

          expected = Just "5.3.2"

      result `shouldBe` expected

    it "handles dash suffixes" $ do
      let input =
            NV.unsafeFromList ["2.0", "2.0-rc2", "2.0-m5", "1.2.9", "2.1-rc1"]

          result = determineLatestRelease input

          expected = Just "2.0"

      result `shouldBe` expected

    it "gracefully produces nothing if no valid versions" $ do
      let input =
            NV.unsafeFromList ["2.0-rc2", "2.0-m5"]

          result = determineLatestRelease input

      result `shouldBe` Nothing

    it "fully invalid versions produce a left" $ do
      let input =
            NV.unsafeFromList ["as//df", "wt//f"]

          result = determineLatestRelease input

      result `shouldBe` Nothing
