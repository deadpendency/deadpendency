module Common.Report.FailureConversion
  ( erroredReasonToReportError,
    generateErroredDependencyReport,
  )
where

import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.Errored.ErroredDependency
import Common.Model.Dependency.Errored.ErroredReason
import Common.Model.Dependency.Registry.DependencyRegistryInfo
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Report.DependencyErrorReason
import Common.Model.Report.DependencyErrorReport
import Common.Model.Report.PackageLink

erroredReasonToReportError :: ErroredReason -> DependencyErrorReason
erroredReasonToReportError =
  \case
    UnexpectedFailureToParseRegistryEntry _ -> DERRegistryUnexpectedStructure
    UnexpectedFailureRegistryDataInconsistent _ -> DERRegistryUnexpectedStructure
    UnexpectedDependencyNameInvalid _ -> DERProcessingFailure
    NoRegistryOrRepoData -> DERNoRegistryOrRepoData

generateErroredDependencyReport :: ErroredDependency -> (ProgrammingLanguage, (DependencyErrorReason, DependencyErrorReport))
generateErroredDependencyReport (ErroredDependency dependencyIdentifier _ programmingLanguage maybeRegistryInfo errorReason) =
  let maybeDependencyName = getDIName dependencyIdentifier
      maybePackageLink = liftA2 getPackageLink maybeDependencyName maybeRegistryInfo
   in ( programmingLanguage,
        ( erroredReasonToReportError errorReason,
          DependencyErrorReport
            { _dependencyIdentifier = dependencyIdentifier,
              _dependencyPackageLink = maybePackageLink
            }
        )
      )

getPackageLink :: DependencyName -> DependencyRegistryInfo -> PackageLink
getPackageLink dependencyName registryInfo =
  PackageLink
    (registryInfo ^. #_registry)
    dependencyName
