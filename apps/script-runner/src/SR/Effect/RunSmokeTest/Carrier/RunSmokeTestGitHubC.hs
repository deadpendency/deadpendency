{-# LANGUAGE DataKinds #-}

module SR.Effect.RunSmokeTest.Carrier.RunSmokeTestGitHubC
  ( RunSmokeTestGitHubIOC (..),
    internalRunSmokeTest,
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Effect.GitHub.InstallationAuth.InstallationAuth
import Common.Model.Config.AppEnv
import Common.Model.Config.CommonConfig
import Common.Model.GitHub.Auth.GHInstallationAuth
import Common.Model.GitHub.GHAppId
import Common.Model.GitHub.GHAppInstallationId
import Control.Algebra (Algebra (..), Has, (:+:) (..))
import Control.Carrier.Reader (Reader, ask)
import Data.Vector qualified as V
import SR.Effect.RunSmokeTest.Backend.Smokes.SmokeDotNet
import SR.Effect.RunSmokeTest.Backend.Smokes.SmokeGolang
import SR.Effect.RunSmokeTest.Backend.Smokes.SmokeHaskell
import SR.Effect.RunSmokeTest.Backend.Smokes.SmokeJavaGradle
import SR.Effect.RunSmokeTest.Backend.Smokes.SmokeJavaMaven
import SR.Effect.RunSmokeTest.Backend.Smokes.SmokeJavaScript
import SR.Effect.RunSmokeTest.Backend.Smokes.SmokePhp
import SR.Effect.RunSmokeTest.Backend.Smokes.SmokePythonPipfile
import SR.Effect.RunSmokeTest.Backend.Smokes.SmokePythonRequirementsTxt
import SR.Effect.RunSmokeTest.Backend.Smokes.SmokeRuby
import SR.Effect.RunSmokeTest.Backend.Smokes.SmokeRust
import SR.Effect.RunSmokeTest.Backend.Smokes.SmokeScalaMaven
import SR.Effect.RunSmokeTest.Backend.Smokes.SmokeUnknownLanguage
import SR.Effect.RunSmokeTest.Model.SmokeResult
import SR.Effect.RunSmokeTest.RunSmokeTest
import Streamly.Prelude qualified as S

newtype RunSmokeTestGitHubIOC m a = RunSmokeTestGitHubIOC {runRunSmokeTestGitHubIOC :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance
  ( Algebra sig m,
    Has AppEventEmit sig m,
    Has InstallationAuth sig m,
    Has (Reader CommonConfig) sig m,
    MonadIO m
  ) =>
  Algebra (RunSmokeTest :+: sig) (RunSmokeTestGitHubIOC m)
  where
  alg hdl sig ctx = case sig of
    (L RunSmokeTest) -> do
      emitAppEventInfo (AppEventMessage "Started: Run Smoke Test ")

      config <- ask @CommonConfig
      let appEnv = config ^. #_appEnv
      let appInstallationId =
            GHAppInstallationId $
              case appEnv of
                Prod -> 11238978
                PreProd -> 11324639
                Test -> 11324639
          appId =
            GHAppId $
              case appEnv of
                Prod -> 51614
                PreProd -> 77327
                Test -> 77327

      installationAuth <- obtainInstallationAuth appInstallationId

      result <- liftIO $ internalRunSmokeTest installationAuth appEnv appId

      emitAppEventInfoA (AppEventMessage "Finished: Run Smoke Test") (AppEventAdditional result)

      RunSmokeTestGitHubIOC $ pure (ctx $> result)
    (R other) -> RunSmokeTestGitHubIOC $ alg (runRunSmokeTestGitHubIOC . hdl) other ctx

internalRunSmokeTest :: GHInstallationAuth -> AppEnv -> GHAppId -> IO (V.Vector SmokeResult)
internalRunSmokeTest installationAuth appEnv appId = do
  let smokesToRun =
        [ smokeDotNet installationAuth appEnv appId,
          smokeHaskell installationAuth appEnv appId,
          smokeJavaGradle installationAuth appEnv appId,
          smokeJavaMaven installationAuth appEnv appId,
          smokeScalaMaven installationAuth appEnv appId,
          smokeJavaScript installationAuth appEnv appId,
          smokePhp installationAuth appEnv appId,
          smokePythonPipfile installationAuth appEnv appId,
          smokePythonRequirementsTxt installationAuth appEnv appId,
          smokeRuby installationAuth appEnv appId,
          smokeRust installationAuth appEnv appId,
          smokeGolang installationAuth appEnv appId,
          smokeUnknownLanguage installationAuth appEnv appId
        ]

  liftIO $ fmap V.fromList $ S.toList $ S.maxThreads 13 $ S.fromAsync $ S.fromFoldableM smokesToRun
