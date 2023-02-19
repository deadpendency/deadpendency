module DD.Effect.DetermineDependencies.Backend.LanguageFiles.Java.PomXml
  ( PomXmlInput (..),
  )
where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.File.DependenciesFileType
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Parsing.Xml
import DD.Effect.DetermineDependencies.Backend.Model.CanDetermineDependencies
import DD.Effect.DetermineDependencies.Model.DetermineDependenciesError
import Data.Text qualified as Text
import Data.Vector qualified as V
import Text.XML.Hexml qualified as HX
import Text.XML.Hexml.Lens qualified as HX

newtype PomXml = PomXml
  { _pjBasicDependencies :: V.Vector BasicDependency
  }
  deriving stock (Eq, Show, Generic)

newtype PomXmlInput = PomXmlInput
  { _pjText :: Text
  }
  deriving stock (Eq, Show, Generic)

parseProjectDepsFromText :: ProgrammingLanguage -> Text -> Either Text (V.Vector BasicDependency)
parseProjectDepsFromText pl input = do
  docNode <- first decodeUtf8 $ HX.parse $ stripCDataSections $ encodeUtf8 input
  let projectNode = docNode ^.. projectFromRoot
      deps = V.fromList $ projectNode ^.. (folded . dependenciesFromParent)
      buildDeps = V.fromList $ projectNode ^.. (folded . buildDependenciesFromProject)
      profilesDeps = V.fromList $ projectNode ^.. (folded . profileDeps)
  pure $ fmap (nameToDependency pl) $ deps <> buildDeps <> profilesDeps

projectFromRoot :: Fold HX.Node HX.Node
projectFromRoot =
  HX.node @Text "project"

profileDeps :: Fold HX.Node (Text, Text)
profileDeps =
  HX.node @Text "profiles"
    . HX.node @Text "profile"
    . dependenciesFromParent

dependenciesFromParent :: Fold HX.Node (Text, Text)
dependenciesFromParent =
  HX.node @Text "dependencies"
    . HX.node @Text "dependency"
    . folding (\s -> zip (s ^.. groupId) (s ^.. artifactId))
    . filtered (\(g, a) -> not (Text.null g) && not (Text.null a))

buildDependenciesFromProject :: Fold HX.Node (Text, Text)
buildDependenciesFromProject =
  HX.node @Text "build"
    . HX.node @Text "plugins"
    . HX.node @Text "plugin"
    . folding (\s -> zip (s ^.. groupId) (s ^.. artifactId))
    . filtered (\(g, a) -> not (Text.null g) && not (Text.null a))

groupId :: Fold HX.Node Text
groupId =
  HX.node @Text "groupId"
    . HX._inner

artifactId :: Fold HX.Node Text
artifactId =
  HX.node @Text "artifactId"
    . HX._inner

nameToDependency :: ProgrammingLanguage -> (Text, Text) -> BasicDependency
nameToDependency pl (nsText, nameText) =
  BasicDependency pl (DependencyIdentifierNamed (DependencyName $ nsText <> "/" <> nameText)) Nothing

instance CanDetermineDependencies PomXmlInput where
  determineDependencies gitPath =
    bimap
      (UnableToParseDependencyFile MavenPomXml gitPath)
      _pjBasicDependencies
      . fmap PomXml
      . parseProjectDepsFromText Java
      . _pjText
