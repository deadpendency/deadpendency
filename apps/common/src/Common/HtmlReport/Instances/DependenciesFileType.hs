{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.HtmlReport.Instances.DependenciesFileType () where

import Common.HtmlReport.HtmlReport
import Common.HtmlReport.HtmlReportDecodeError
import Common.Model.Dependency.File.DependenciesFileType

instance ToHtmlReportBody DependenciesFileType where
  toHtmlReportBody =
    \case
      PipRequirementsTxt -> "requirements.txt"
      PythonSetupPy -> "setup.py"
      PipenvPipfile -> "Pipfile"
      PyProjectToml -> "pyproject.toml"
      NpmPackageJson -> "package.json"
      PackagistComposerJson -> "composer.json"
      BundlerGemfile -> "Gemfile"
      RubyGemsGemspec -> "*.gemspec"
      HpackPackageYaml -> "package.yaml"
      HaskellCabal -> "*.cabal"
      CratesCargoToml -> "Cargo.toml"
      DotNetCSharpProject -> "*.csproj"
      DotNetVisualBasicProject -> "*.vbproj"
      MavenPomXml -> "pom.xml"
      BuildGradle -> "build.gradle"
      BuildSbt -> "build.sbt"
      GoMod -> "go.mod"

instance FromHtmlReportBody DependenciesFileType where
  fromHtmlReportBody =
    \case
      "requirements.txt" -> Right PipRequirementsTxt
      "setup.py" -> Right PythonSetupPy
      "Pipfile" -> Right PipenvPipfile
      "pyproject.toml" -> Right PyProjectToml
      "package.json" -> Right NpmPackageJson
      "composer.json" -> Right PackagistComposerJson
      "Gemfile" -> Right BundlerGemfile
      "*.gemspec" -> Right RubyGemsGemspec
      "package.yaml" -> Right HpackPackageYaml
      "*.cabal" -> Right HaskellCabal
      "Cargo.toml" -> Right CratesCargoToml
      "*.csproj" -> Right DotNetCSharpProject
      "*.vbproj" -> Right DotNetVisualBasicProject
      "pom.xml" -> Right MavenPomXml
      "build.gradle" -> Right BuildGradle
      "build.sbt" -> Right BuildSbt
      "go.mod" -> Right GoMod
      other -> Left $ HtmlReportDecodeError $ "Unxpected DependenciesFileType of " <> other
