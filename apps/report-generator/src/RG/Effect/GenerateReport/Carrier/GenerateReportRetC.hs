module RG.Effect.GenerateReport.Carrier.GenerateReportRetC
  ( runGenerateReportRet,
  )
where

import Control.Algebra (Algebra (..), (:+:) (..))
import Control.Carrier.Reader (ReaderC, ask, runReader)
import Control.Carrier.Writer.Strict (WriterC, runWriter)
import Control.Effect.Writer (tell)
import RG.Effect.GenerateReport.GenerateReport
import RG.Effect.GenerateReport.Model.GenerateReportRequest
import RG.Effect.GenerateReport.Model.GenerateReportResult

newtype GenerateReportRetC m a = GenerateReportRetC {runGenerateReportRetC :: ReaderC GenerateReportResult (WriterC [GenerateReportRequest] m) a}
  deriving newtype (Functor, Applicative, Monad)

instance (Algebra sig m) => Algebra (GenerateReport :+: sig) (GenerateReportRetC m) where
  alg hdl sig ctx = case sig of
    (L (GenerateReport request)) -> GenerateReportRetC $ tell [request] *> ask <&> (ctx $>)
    (R other) -> GenerateReportRetC $ alg (runGenerateReportRetC . hdl) (R (R other)) ctx

runGenerateReportRet :: GenerateReportResult -> GenerateReportRetC m a -> m ([GenerateReportRequest], a)
runGenerateReportRet result = runWriter . runReader result . runGenerateReportRetC
