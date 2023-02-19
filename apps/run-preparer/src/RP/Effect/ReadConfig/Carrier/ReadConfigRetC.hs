module RP.Effect.ReadConfig.Carrier.ReadConfigRetC
  ( runReadConfigRet,
  )
where

import Control.Algebra (Algebra (..), (:+:) (..))
import Control.Carrier.Reader (ReaderC, ask, runReader)
import Control.Carrier.Writer.Strict (WriterC, runWriter)
import Control.Effect.Writer (tell)
import RP.Effect.ReadConfig.Model.ReadConfigRequest
import RP.Effect.ReadConfig.Model.ReadConfigResult
import RP.Effect.ReadConfig.ReadConfig (ReadConfig (..))

newtype ReadConfigRetC m a = ReadConfigRetC {runReadConfigRetC :: ReaderC ReadConfigResult (WriterC [ReadConfigRequest] m) a}
  deriving newtype (Functor, Applicative, Monad)

instance (Algebra sig m) => Algebra (ReadConfig :+: sig) (ReadConfigRetC m) where
  alg hdl sig ctx = case sig of
    (L (ConfigRead request)) -> ReadConfigRetC $ tell [request] *> ask <&> (ctx $>)
    (R other) -> ReadConfigRetC $ alg (runReadConfigRetC . hdl) (R (R other)) ctx

runReadConfigRet :: ReadConfigResult -> ReadConfigRetC m a -> m ([ReadConfigRequest], a)
runReadConfigRet readConfigResult = runWriter . runReader readConfigResult . runReadConfigRetC
