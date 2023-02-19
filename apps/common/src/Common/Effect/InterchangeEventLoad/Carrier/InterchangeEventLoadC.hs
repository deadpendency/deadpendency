module Common.Effect.InterchangeEventLoad.Carrier.InterchangeEventLoadC
  ( InterchangeEventLoadC (..),
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Effect.InterchangeEventDecode.InterchangeEventDecode
import Common.Effect.InterchangeEventLoad.InterchangeEventLoad (InterchangeEventLoad (..))
import Common.Model.Details.Run
import Common.Model.Details.RunTrace
import Common.Model.GitHub.Auth.GHInstallationAuth
import Common.Model.GitHub.Checks.CheckRun
import Common.Model.InterchangeEvent.InterchangeEvent
import Common.Model.RepoConfig.RepoConfig
import Control.Algebra (Algebra (..), Has, (:+:) (..))
import Control.Effect.State (State, put)
import Data.Aeson

newtype InterchangeEventLoadC (t :: Type) m a = InterchangeEventLoadC {runInterchangeEventLoadC :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance
  ( Algebra sig m,
    Has AppEventEmit sig m,
    Has (State (Maybe RunTrace)) sig m,
    Has (State (Maybe Run)) sig m,
    Has (State (Maybe CheckRun)) sig m,
    Has (State (Maybe RepoConfig)) sig m,
    Has (State (Maybe GHInstallationAuth)) sig m,
    Has (InterchangeEventDecode (InterchangeEvent t)) sig m,
    FromJSON t,
    ToJSON t
  ) =>
  Algebra (InterchangeEventLoad t :+: sig) (InterchangeEventLoadC t m)
  where
  alg hdl sig ctx = case sig of
    (L (LoadInterchangeEvent rm)) -> do
      emitAppEventInfo (AppEventMessage "Started: Interchange Event Load")
      interchangeEvent <- decodeInterchangeEvent rm
      let run = interchangeEvent ^. #_run
          runTrace = run ^. #_runTrace
          checkRun = interchangeEvent ^. #_checkRun
          ghInstallationAuth = interchangeEvent ^. #_ghInstallationAuth
          repoConfig = interchangeEvent ^. #_repoConfig
      put $ Just run
      put $ Just runTrace
      put $ Just checkRun
      put $ Just ghInstallationAuth
      put $ Just repoConfig
      emitAppEventInfo (AppEventMessage "Finished: Interchange Event Load")
      InterchangeEventLoadC $ pure (ctx $> interchangeEvent)
    (R other) -> InterchangeEventLoadC $ alg (runInterchangeEventLoadC . hdl) other ctx
