module Effect.VerifyPlan.Backend.VerifyPlanBackendSpec (spec) where

import Common.Effect.CacheExternal.Carrier.CacheExternalRetC
import Common.Effect.GitHub.CountPrivateInstalls.Carrier.CountPrivateInstallsRetC
import Common.Effect.GitHub.CountPrivateInstalls.Model.CountPrivateInstallsResult
import Common.Effect.GitHub.DetermineAccountPlan.Carrier.DetermineAccountPlanRetC
import Common.Effect.GitHub.DetermineAccountPlan.Model.DetermineAccountPlanResult
import Common.Model.Config.AppEnv
import Common.Model.Plan.Plan
import Common.Model.Plan.PlanError
import RP.Effect.VerifyPlan.Backend.VerifyPlanBackend
import Test.Hspec

spec :: Spec
spec = parallel $ do
  context "Verifying Plan" $ do
    it "non prod passes" $ do
      let isDeadpendencyRun = True
          appEnv = Prod
          isPrivate = True
          cacheKey = "cache-key"
          cachedResult = Nothing @()
          privateInstallsCount = CountPrivateInstallsResult 100
          accountPlan = DetermineAccountPlanResult IndividualDeveloperPlan

      let runHandler =
            runCacheExternalRet cachedResult
              >>> runCountPrivateInstallsRet privateInstallsCount
              >>> runDetermineAccountPlanRet accountPlan
              >>> runIdentity

      let handlerResult = validatePlan isDeadpendencyRun appEnv isPrivate cacheKey
          (writtenToCache, result) = runHandler handlerResult

      let expectedResult = Nothing
          expectedCacheWrites = []

      result `shouldBe` expectedResult
      writtenToCache `shouldBe` expectedCacheWrites

    it "non prod passes" $ do
      let isDeadpendencyRun = False
          appEnv = PreProd
          isPrivate = True
          cacheKey = "cache-key"
          cachedResult = Nothing @()
          privateInstallsCount = CountPrivateInstallsResult 100
          accountPlan = DetermineAccountPlanResult IndividualDeveloperPlan

      let runHandler =
            runCacheExternalRet cachedResult
              >>> runCountPrivateInstallsRet privateInstallsCount
              >>> runDetermineAccountPlanRet accountPlan
              >>> runIdentity

      let handlerResult = validatePlan isDeadpendencyRun appEnv isPrivate cacheKey
          (writtenToCache, result) = runHandler handlerResult

      let expectedResult = Nothing
          expectedCacheWrites = []

      result `shouldBe` expectedResult
      writtenToCache `shouldBe` expectedCacheWrites

    it "repo is public is no failure" $ do
      let isDeadpendencyRun = False
          appEnv = Prod
          isPrivate = False
          cacheKey = "cache-key"
          cachedResult = Nothing @()
          privateInstallsCount = CountPrivateInstallsResult 100
          accountPlan = DetermineAccountPlanResult IndividualDeveloperPlan

      let runHandler =
            runCacheExternalRet cachedResult
              >>> runCountPrivateInstallsRet privateInstallsCount
              >>> runDetermineAccountPlanRet accountPlan
              >>> runIdentity

      let handlerResult = validatePlan isDeadpendencyRun appEnv isPrivate cacheKey
          (writtenToCache, result) = runHandler handlerResult

      let expectedResult = Nothing
          expectedCacheWrites = []

      result `shouldBe` expectedResult
      writtenToCache `shouldBe` expectedCacheWrites

    it "repo is private + invalid but cached pass" $ do
      let isDeadpendencyRun = False
          appEnv = Prod
          isPrivate = True
          cacheKey = "cache-key"
          cachedResult = Just ()
          privateInstallsCount = CountPrivateInstallsResult 100
          accountPlan = DetermineAccountPlanResult IndividualDeveloperPlan

      let runHandler =
            runCacheExternalRet cachedResult
              >>> runCountPrivateInstallsRet privateInstallsCount
              >>> runDetermineAccountPlanRet accountPlan
              >>> runIdentity

      let handlerResult = validatePlan isDeadpendencyRun appEnv isPrivate cacheKey
          (writtenToCache, result) = runHandler handlerResult

      let expectedResult = Nothing
          expectedCacheWrites = []

      result `shouldBe` expectedResult
      writtenToCache `shouldBe` expectedCacheWrites

    it "failing a plan" $ do
      let isDeadpendencyRun = False
          appEnv = Prod
          isPrivate = True
          cacheKey = "cache-key"
          cachedResult = Nothing @()
          privateInstallsCount = CountPrivateInstallsResult 100
          accountPlan = DetermineAccountPlanResult IndividualDeveloperPlan

      let runHandler =
            runCacheExternalRet cachedResult
              >>> runCountPrivateInstallsRet privateInstallsCount
              >>> runDetermineAccountPlanRet accountPlan
              >>> runIdentity

      let handlerResult = validatePlan isDeadpendencyRun appEnv isPrivate cacheKey
          (writtenToCache, result) = runHandler handlerResult

      let expectedResult = Just $ ExceededPlan IndividualDeveloperPlan 100
          expectedCacheWrites = []

      result `shouldBe` expectedResult
      writtenToCache `shouldBe` expectedCacheWrites

    it "exact count as plan limit should pass" $ do
      let isDeadpendencyRun = False
          appEnv = Prod
          isPrivate = True
          cacheKey = "cache-key"
          cachedResult = Nothing @()
          privateInstallsCount = CountPrivateInstallsResult 5
          accountPlan = DetermineAccountPlanResult IndividualDeveloperPlan

      let runHandler =
            runCacheExternalRet cachedResult
              >>> runCountPrivateInstallsRet privateInstallsCount
              >>> runDetermineAccountPlanRet accountPlan
              >>> runIdentity

      let handlerResult = validatePlan isDeadpendencyRun appEnv isPrivate cacheKey
          (writtenToCache, result) = runHandler handlerResult

      let expectedResult = Nothing
          expectedCacheWrites = [(604800, "cache-key")]

      result `shouldBe` expectedResult
      writtenToCache `shouldBe` expectedCacheWrites

    it "beta always passes" $ do
      let isDeadpendencyRun = False
          appEnv = Prod
          isPrivate = True
          cacheKey = "cache-key"
          cachedResult = Nothing @()
          privateInstallsCount = CountPrivateInstallsResult 5000
          accountPlan = DetermineAccountPlanResult BetaPlan

      let runHandler =
            runCacheExternalRet cachedResult
              >>> runCountPrivateInstallsRet privateInstallsCount
              >>> runDetermineAccountPlanRet accountPlan
              >>> runIdentity

      let handlerResult = validatePlan isDeadpendencyRun appEnv isPrivate cacheKey
          (writtenToCache, result) = runHandler handlerResult

      let expectedResult = Nothing
          expectedCacheWrites = [(604800, "cache-key")]

      result `shouldBe` expectedResult
      writtenToCache `shouldBe` expectedCacheWrites

    it "zero limit works" $ do
      let isDeadpendencyRun = False
          appEnv = Prod
          isPrivate = True
          cacheKey = "cache-key"
          cachedResult = Nothing @()
          privateInstallsCount = CountPrivateInstallsResult 1
          accountPlan = DetermineAccountPlanResult OpenSourceOrganizationPlan

      let runHandler =
            runCacheExternalRet cachedResult
              >>> runCountPrivateInstallsRet privateInstallsCount
              >>> runDetermineAccountPlanRet accountPlan
              >>> runIdentity

      let handlerResult = validatePlan isDeadpendencyRun appEnv isPrivate cacheKey
          (writtenToCache, result) = runHandler handlerResult

      let expectedResult = Just $ ExceededPlan OpenSourceOrganizationPlan 1
          expectedCacheWrites = []

      result `shouldBe` expectedResult
      writtenToCache `shouldBe` expectedCacheWrites
