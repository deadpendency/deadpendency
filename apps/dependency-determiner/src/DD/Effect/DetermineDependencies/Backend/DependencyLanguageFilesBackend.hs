module DD.Effect.DetermineDependencies.Backend.DependencyLanguageFilesBackend
  ( determineDependencyLanguageFiles,
    loadFileDeps,
  )
where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.File.DependenciesFileLoad
import Common.Model.Dependency.File.DependenciesFileLoadDetails
import Common.Model.Dependency.File.DependenciesFileType
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Git.GitFileMatch
import Common.Model.GitHub.GHRepoFile
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.DotNet.CSharpNetProject
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.DotNet.VisualBasicNetProject
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.Golang.GoMod
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.Haskell.HaskellCabal
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.Haskell.PackageYaml
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.Java.BuildGradle
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.Java.PomXml
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.JavaScript.PackageJson
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.Php.ComposerJson
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.Python.Pipfile
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.Python.PyProjectToml
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.Python.RequirementsTxt
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.Python.SetupPy
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.Ruby.Gemfile
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.Ruby.Gemspec
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.Rust.CargoManifest
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.Scala.BuildSbt
import DD.Effect.DetermineDependencies.Backend.Model.CanDetermineDependencies
import DD.Effect.DetermineDependencies.Backend.Model.RawDepFileContent
import DD.Effect.DetermineDependencies.Model.DetermineDependenciesError
import Data.Vector qualified as V

determineDependencyLanguageFiles :: ProgrammingLanguage -> V.Vector DependenciesFileLoad
determineDependencyLanguageFiles =
  \case
    JavaScript -> npmDeps
    TypeScript -> npmDeps
    Python ->
      V.singleton
        (DependenciesFileLoad PipRequirementsTxt (DFLDSearch $ GitFileMatch "requirements*.txt"))
        `V.snoc` DependenciesFileLoad PipenvPipfile (DFLDSearch $ GitFileMatch "Pipfile")
        `V.snoc` DependenciesFileLoad PyProjectToml (DFLDSearch $ GitFileMatch "pyproject.toml")
        `V.snoc` DependenciesFileLoad PythonSetupPy (DFLDSearch $ GitFileMatch "setup.py")
    Php -> V.singleton (DependenciesFileLoad PackagistComposerJson (DFLDSearch $ GitFileMatch "composer.json"))
    Ruby ->
      V.singleton (DependenciesFileLoad BundlerGemfile (DFLDSearch $ GitFileMatch "Gemfile"))
        `V.snoc` DependenciesFileLoad RubyGemsGemspec (DFLDSearch $ GitFileMatch "*.gemspec")
    Haskell ->
      V.singleton (DependenciesFileLoad HpackPackageYaml (DFLDSearch $ GitFileMatch "package.yaml"))
        `V.snoc` DependenciesFileLoad HaskellCabal (DFLDSearch $ GitFileMatch "*.cabal")
    Rust -> V.singleton (DependenciesFileLoad CratesCargoToml (DFLDSearch $ GitFileMatch "Cargo.toml"))
    CSharpNet -> V.singleton (DependenciesFileLoad DotNetCSharpProject (DFLDSearch $ GitFileMatch "*.csproj"))
    VisualBasicNet -> V.singleton (DependenciesFileLoad DotNetVisualBasicProject (DFLDSearch $ GitFileMatch "*.vbproj"))
    Java -> mavenDeps
    Kotlin -> mavenDeps `V.snoc` DependenciesFileLoad BuildGradle (DFLDSearch $ GitFileMatch "build.gradle.kts")
    Scala -> mavenDeps `V.snoc` DependenciesFileLoad BuildSbt (DFLDSearch $ GitFileMatch "build.sbt")
    Golang -> V.singleton (DependenciesFileLoad GoMod (DFLDSearch $ GitFileMatch "go.mod"))
    UnsupportedLanguage _ -> V.empty

npmDeps :: V.Vector DependenciesFileLoad
npmDeps = V.singleton (DependenciesFileLoad NpmPackageJson (DFLDSearch $ GitFileMatch "package.json"))

mavenDeps :: V.Vector DependenciesFileLoad
mavenDeps =
  V.singleton (DependenciesFileLoad MavenPomXml (DFLDSearch $ GitFileMatch "pom.xml"))
    `V.snoc` DependenciesFileLoad BuildGradle (DFLDSearch $ GitFileMatch "build.gradle")

loadFileDeps :: RawDepFileContent -> Either DetermineDependenciesError (V.Vector BasicDependency)
loadFileDeps (RawDepFileContent depFileType ghRepoFile) =
  repoFileToInput depFileType ghRepoFile & \(CDDWrapper gitPath a) -> determineDependencies gitPath a

repoFileToInput :: DependenciesFileType -> GHRepoFile -> CDDWrapper
repoFileToInput fileType (GHRepoFile gitPath fileContents) =
  case fileType of
    PipRequirementsTxt -> CDDWrapper gitPath $ RequirementsTxtInput fileContents
    PythonSetupPy -> CDDWrapper gitPath $ SetupPyInput fileContents
    PipenvPipfile -> CDDWrapper gitPath $ PipfileInput fileContents
    PyProjectToml -> CDDWrapper gitPath $ PyProjectTomlInput fileContents
    NpmPackageJson -> CDDWrapper gitPath $ PackageJsonInput fileContents
    PackagistComposerJson -> CDDWrapper gitPath $ ComposerJsonInput fileContents
    BundlerGemfile -> CDDWrapper gitPath $ GemfileInput fileContents
    RubyGemsGemspec -> CDDWrapper gitPath $ GemspecInput fileContents
    HpackPackageYaml -> CDDWrapper gitPath $ PackageYamlInput fileContents
    HaskellCabal -> CDDWrapper gitPath $ HaskellCabalInput fileContents
    CratesCargoToml -> CDDWrapper gitPath $ CargoManifestInput fileContents
    DotNetCSharpProject -> CDDWrapper gitPath $ CSharpNetProjectInput fileContents
    DotNetVisualBasicProject -> CDDWrapper gitPath $ VisualBasicNetProjectInput fileContents
    MavenPomXml -> CDDWrapper gitPath $ PomXmlInput fileContents
    BuildGradle -> CDDWrapper gitPath $ BuildGradleInput fileContents
    BuildSbt -> CDDWrapper gitPath $ BuildSbtInput fileContents
    GoMod -> CDDWrapper gitPath $ GoModInput fileContents
