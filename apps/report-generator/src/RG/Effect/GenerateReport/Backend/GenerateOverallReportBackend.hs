module RG.Effect.GenerateReport.Backend.GenerateOverallReportBackend
  ( generateOverallReport,
  )
where

import Common.Model.Assessment.DependencyAssessment
import Common.Model.Config.AppVersion
import Common.Model.Dependency.Errored.ErroredDependency
import Common.Model.Dependency.Ignored.IgnoredDependency
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Report.DependencyErrorReason
import Common.Model.Report.DependencyErrorReport
import Common.Model.Report.DependencyErrorReports
import Common.Model.Report.DependencyLanguageReport
import Common.Model.Report.DependencyReports
import Common.Model.Report.OverallReport
import Common.Model.Report.ReportResult
import Common.Report.FailureConversion
import Data.Map.Strict qualified as M
import Data.Vector qualified as V
import Data.Vector.NonEmpty qualified as NV
import RG.Effect.GenerateReport.Backend.GenerateDependencyReportBackend

generateOverallReport :: AppVersion -> V.Vector ErroredDependency -> V.Vector IgnoredDependency -> NV.NonEmptyVector DependencyAssessment -> OverallReport
generateOverallReport appVersion erroredDeps ignoredDeps dependencyAssessments =
  let dependencyReports = createDependencyReports erroredDeps ignoredDeps dependencyAssessments
      reportResult =
        if
            | hasFailures dependencyReports -> ReportResultFail
            | hasWarnings dependencyReports -> ReportResultWarning
            | otherwise -> ReportResultPass
   in OverallReport
        { _producedWithVersion = appVersion,
          _reportResult = reportResult,
          _dependencyReports = dependencyReports,
          _isTruncated = False
        }

hasFailures :: DependencyReports -> Bool
hasFailures (DRSingleLanguageReports _ _ failures _ _) = not $ V.null failures
hasFailures (DRMultiLanguageReports _ _ failures _ _) = not $ V.null failures

hasWarnings :: DependencyReports -> Bool
hasWarnings (DRSingleLanguageReports _ warnings _ _ _) = not $ V.null warnings
hasWarnings (DRMultiLanguageReports _ warnings _ _ _) = not $ V.null warnings

createDependencyReports :: V.Vector ErroredDependency -> V.Vector IgnoredDependency -> NV.NonEmptyVector DependencyAssessment -> DependencyReports
createDependencyReports erroredDeps ignoredDeps dependencyAssessments =
  let passPlReports = tupleToKeyed $ NV.mapMaybe toMaybePass dependencyAssessments
      warnPlReports = tupleToKeyed $ NV.mapMaybe toMaybeWarning dependencyAssessments
      failPlReports = tupleToKeyed $ NV.mapMaybe toMaybeFailure dependencyAssessments
      errorPlReports = errorReasonsToKeyedWithSort <<$>> tupleToKeyed (erroredDeps <&> generateErroredDependencyReport)
      ignorePLReports = tupleToKeyed $ toIgnoreReport <$> ignoredDeps
      allLanguages =
        fmap fst passPlReports
          V.++ fmap fst warnPlReports
          V.++ fmap fst failPlReports
          V.++ fmap fst errorPlReports
      isMultiLanguage =
        length (ordNubV allLanguages) > 1
   in if isMultiLanguage
        then DRMultiLanguageReports (fmap toDepLang passPlReports) (fmap toDepLang warnPlReports) (fmap toDepLang failPlReports) (fmap toDepLang ignorePLReports) (fmap toDepLang errorPlReports)
        else DRSingleLanguageReports (getFirstV passPlReports) (getFirstV warnPlReports) (getFirstV failPlReports) (getFirstV ignorePLReports) (getFirstV errorPlReports)

tupleToKeyed :: (Ord key) => V.Vector (key, a) -> V.Vector (key, NV.NonEmptyVector a)
tupleToKeyed =
  V.fromList
    . M.toList
    . V.foldl'
      (\map' (pl, report) -> M.insertWith (NV.++) pl (NV.singleton report) map')
      M.empty

errorReasonsToKeyedWithSort :: NV.NonEmptyVector (DependencyErrorReason, DependencyErrorReport) -> NV.NonEmptyVector DependencyErrorReports
errorReasonsToKeyedWithSort = NV.unsafeFromVector . fmap (uncurry DependencyErrorReports . fmap sortNV) . tupleToKeyed . NV.toVector

toDepLang :: (Ord a) => (ProgrammingLanguage, NV.NonEmptyVector a) -> DependencyLanguageReport a
toDepLang = uncurry DependencyLanguageReport . fmap sortNV

getFirstV :: (Ord a) => V.Vector (ProgrammingLanguage, NV.NonEmptyVector a) -> V.Vector a
getFirstV inputVec =
  if V.null inputVec
    then V.empty
    else sortV $ NV.toVector $ snd $ V.head inputVec
