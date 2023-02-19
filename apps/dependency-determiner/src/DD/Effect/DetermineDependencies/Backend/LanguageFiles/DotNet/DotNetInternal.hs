module DD.Effect.DetermineDependencies.Backend.LanguageFiles.DotNet.DotNetInternal
  ( parseProjectDepsFromText,
  )
where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Ecosystem.ProgrammingLanguage
import Data.Vector qualified as V
import Text.XML.Hexml qualified as HX
import Text.XML.Hexml.Lens qualified as HX

parseProjectDepsFromText :: ProgrammingLanguage -> Text -> Either Text (V.Vector BasicDependency)
parseProjectDepsFromText pl input = do
  docNode <- first decodeUtf8 $ HX.parse (encodeUtf8 input)
  pure $ fmap (nameToDependency pl) $ V.fromList $ docNode ^.. packageReferencesFromRoot

packageReferencesFromRoot :: Fold HX.Node Text
packageReferencesFromRoot =
  HX.node @Text "Project"
    . HX.node @Text "ItemGroup"
    . HX.node @Text "PackageReference"
    . HX._Attribute @Text "Include"
    . _Just

nameToDependency :: ProgrammingLanguage -> Text -> BasicDependency
nameToDependency pl name = BasicDependency pl (DependencyIdentifierNamed $ DependencyName name) Nothing
