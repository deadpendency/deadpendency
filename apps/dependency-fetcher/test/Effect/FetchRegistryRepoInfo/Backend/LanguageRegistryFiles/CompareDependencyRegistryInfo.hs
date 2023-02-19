module Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.CompareDependencyRegistryInfo
  ( CompareDependencyRegistryInfo (..),
  )
where

import Common.Model.Dependency.Registry.DependencyRegistryInfo

newtype CompareDependencyRegistryInfo = CompareDependencyRegistryInfo
  { _info :: DependencyRegistryInfo
  }
  deriving stock (Show, Generic)

instance Eq CompareDependencyRegistryInfo where
  (==) (CompareDependencyRegistryInfo (DependencyRegistryInfo registry maybeRepo deprecationStatus maybeReleaseTime)) (CompareDependencyRegistryInfo (DependencyRegistryInfo registry' maybeRepo' deprecationStatus' maybeReleaseTime')) =
    registry == registry'
      && maybeRepo == maybeRepo'
      && deprecationStatus == deprecationStatus'
      && (isJust maybeReleaseTime == isJust maybeReleaseTime')
