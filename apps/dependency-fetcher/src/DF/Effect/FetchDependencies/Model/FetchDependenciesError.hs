{-# LANGUAGE DeriveAnyClass #-}

module DF.Effect.FetchDependencies.Model.FetchDependenciesError
  ( FetchDependenciesError (..),
  )
where

import Common.Aeson.Aeson
import Common.GitHub.Model.GitHubError
import Common.Model.Dependency.Errored.ErroredDependency
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Error.ConsideredAppFailure
import Common.Model.Error.ProcessingError
import Common.Model.Error.ToProcessingError
import Common.Model.Error.UserError
import Common.Model.Report.DependencyErrorReason
import Common.Model.Report.DependencyErrorReport
import Common.Model.Report.DependencyErrorReports
import Common.Model.Report.DependencyLanguageReport
import Common.Report.FailureConversion
import DF.Effect.FetchDependencies.Model.FetchDependencyRegistryException
import Data.Aeson
import Data.Map.Strict qualified as M
import Data.Vector.NonEmpty qualified as NV

data FetchDependenciesError
  = RepoStatsFetchError GitHubError
  | RegistryFetchError FetchDependencyRegistryException
  | UnexpectedEmptyDependenciesInStream
  | UnexpectedEmptyAfterProcessing
  | AllDepsFailedFetch (NV.NonEmptyVector ErroredDependency)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToProcessingError FetchDependenciesError where
  toProcessingError =
    \case
      (RepoStatsFetchError _) -> Nothing
      (RegistryFetchError _) -> Nothing
      UnexpectedEmptyDependenciesInStream -> Nothing
      UnexpectedEmptyAfterProcessing -> Nothing
      AllDepsFailedFetch nvErroredDeps ->
        let plKeyed = tupleToKeyed (nvErroredDeps <&> generateErroredDependencyReport)
            errorReasonKeyed = errorReasonsToKeyedWithSort <<$>> plKeyed
            final = toDepLang <$> errorReasonKeyed
         in Just $ ProcessingErrorUser $ UserErrorAllDepsFailedFetch final

-- bit of an ugly copy from RG.Effect.GenerateReport.Backend.GenerateOverallReportBackend

toDepLang :: (Ord a) => (ProgrammingLanguage, NV.NonEmptyVector a) -> DependencyLanguageReport a
toDepLang = uncurry DependencyLanguageReport . fmap sortNV

tupleToKeyed :: (Ord key) => NV.NonEmptyVector (key, a) -> NV.NonEmptyVector (key, NV.NonEmptyVector a)
tupleToKeyed =
  NV.unsafeFromList
    . M.toList
    . NV.foldl'
      (\map' (pl, report) -> M.insertWith (NV.++) pl (NV.singleton report) map')
      M.empty

errorReasonsToKeyedWithSort :: NV.NonEmptyVector (DependencyErrorReason, DependencyErrorReport) -> NV.NonEmptyVector DependencyErrorReports
errorReasonsToKeyedWithSort = fmap (uncurry DependencyErrorReports . fmap sortNV) . tupleToKeyed

instance ConsideredAppFailure FetchDependenciesError where
  consideredAppFailure =
    \case
      RepoStatsFetchError e -> consideredAppFailure e
      RegistryFetchError e -> consideredAppFailure e
      UnexpectedEmptyDependenciesInStream -> True
      UnexpectedEmptyAfterProcessing -> True
      AllDepsFailedFetch _ -> False

instance ToJSON FetchDependenciesError where
  toJSON = genericToJSON cleanJSONOptions
