module Spec.SmokeAllSpec (spec) where

import Common.Model.Config.AppEnv
import Common.Model.GitHub.Auth.GHInstallationAuth
import Common.Model.GitHub.GHAppId
import CommonTest.GitHub.TestGitHubAuth
import Data.Vector qualified as V
import SR.Effect.RunSmokeTest.Carrier.RunSmokeTestGitHubC (internalRunSmokeTest)
import SR.Effect.RunSmokeTest.Model.SmokeResult
import System.Environment (getEnv)
import System.IO.Error (ioError, userError)
import Test.Hspec

spec :: Spec
spec = parallel $
  context "Smoke All" $
    beforeAll prepareSmokeTest $
      it "produce the expected output" $ \(installAuth, appEnv, appId) -> do
        results <- internalRunSmokeTest installAuth appEnv appId

        let nonSuccesses = V.filter notSuccess results

        nonSuccesses `shouldBe` V.empty

notSuccess :: SmokeResult -> Bool
notSuccess SRSuccess = False
notSuccess _ = True

prepareSmokeTest :: IO (GHInstallationAuth, AppEnv, GHAppId)
prepareSmokeTest = do
  appEnv <- flip whenNothingM (ioError (userError "Error - APP_ENV env variable unavailable or invalid")) $ getEnv "APP_ENV" <&> (appEnvFromText . pack)
  auth <- reloadOrGenerateAuth appEnv
  let appId =
        GHAppId $
          case appEnv of
            Prod -> 51614
            PreProd -> 77327
            Test -> 77327
  pure (auth, appEnv, appId)
