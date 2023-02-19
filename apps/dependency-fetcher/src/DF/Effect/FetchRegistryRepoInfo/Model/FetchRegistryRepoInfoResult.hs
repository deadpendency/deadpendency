module DF.Effect.FetchRegistryRepoInfo.Model.FetchRegistryRepoInfoResult (FetchRegistryRepoInfoResult) where

import Common.Model.Dependency.Errored.ErroredDependency
import DF.Effect.Model.FetchRegistryWithRepo

type FetchRegistryRepoInfoResult =
  Either ErroredDependency FetchRegistryWithRepo
