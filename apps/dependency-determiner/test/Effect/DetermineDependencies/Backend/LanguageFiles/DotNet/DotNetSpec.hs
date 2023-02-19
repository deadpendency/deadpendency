module Effect.DetermineDependencies.Backend.LanguageFiles.DotNet.DotNetSpec (spec) where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Git.GitPath
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.DotNet.CSharpNetProject
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.DotNet.VisualBasicNetProject
import DD.Effect.DetermineDependencies.Backend.Model.CanDetermineDependencies
import Data.Vector qualified as V
import Test.Hspec
import Text.RawString.QQ

spec :: Spec
spec = parallel $
  context "determine deps" $
    describe "of a CSharpNetProject" $ do
      it "determines happy day dependencies" $ do
        let inputRequirements =
              [r|
<Project Sdk="Microsoft.NET.Sdk">
  <ItemGroup>
    <PackageReference Include="Microsoft.CodeAnalysis.FxCopAnalyzers" />
    <PackageReference Include="Microsoft.SourceLink.GitHub" PrivateAssets="All" />
    <PackageReference Include="Microsoft.Net.Compilers.Toolset" />
  </ItemGroup>
</Project>
|]
            input = CSharpNetProjectInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency CSharpNet (DependencyIdentifierNamed $ DependencyName "Microsoft.CodeAnalysis.FxCopAnalyzers") Nothing,
                    BasicDependency CSharpNet (DependencyIdentifierNamed $ DependencyName "Microsoft.SourceLink.GitHub") Nothing,
                    BasicDependency CSharpNet (DependencyIdentifierNamed $ DependencyName "Microsoft.Net.Compilers.Toolset") Nothing
                  ]

        result `shouldBe` expected

      it "parses all deps from multiple ItemGroup blocks" $ do
        let inputRequirements =
              [r|
<Project Sdk="Microsoft.NET.Sdk">
  <ItemGroup>
    <PackageReference Include="Microsoft.CodeAnalysis.FxCopAnalyzers" />
    <PackageReference Include="Microsoft.SourceLink.GitHub" PrivateAssets="All" />
    <PackageReference Include="Microsoft.Net.Compilers.Toolset" />
  </ItemGroup>
  <ItemGroup Condition="'$(TargetFramework)' == 'netstandard1.3'">
    <PackageReference Include="System.Runtime.Serialization.Formatters" Version="$(SystemRuntimeSerializationFormattersPackageVersion)" />
    <PackageReference Include="System.Xml.XmlDocument" Version="$(SystemXmlXmlDocumentPackageVersion)" />
  </ItemGroup>
</Project>
|]
            input = CSharpNetProjectInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency CSharpNet (DependencyIdentifierNamed $ DependencyName "Microsoft.CodeAnalysis.FxCopAnalyzers") Nothing,
                    BasicDependency CSharpNet (DependencyIdentifierNamed $ DependencyName "Microsoft.SourceLink.GitHub") Nothing,
                    BasicDependency CSharpNet (DependencyIdentifierNamed $ DependencyName "Microsoft.Net.Compilers.Toolset") Nothing,
                    BasicDependency CSharpNet (DependencyIdentifierNamed $ DependencyName "System.Runtime.Serialization.Formatters") Nothing,
                    BasicDependency CSharpNet (DependencyIdentifierNamed $ DependencyName "System.Xml.XmlDocument") Nothing
                  ]

        result `shouldBe` expected

      it "works with empty ItemGroups" $ do
        let inputRequirements =
              [r|
<Project Sdk="Microsoft.NET.Sdk">
  <ItemGroup>
  </ItemGroup>
</Project>
|]
            input = CSharpNetProjectInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right V.empty

        result `shouldBe` expected

      it "works with random xml" $ do
        let inputRequirements =
              [r|
<Wah/>
|]
            input = CSharpNetProjectInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right V.empty

        result `shouldBe` expected

      it "invalid xml produces a left" $ do
        let inputRequirements =
              [r|
askdfhaskdjfh as kdjfh ><
|]
            input = CSharpNetProjectInput inputRequirements
            result = determineDependencies (GitPath "path") input

        isLeft result `shouldBe` True

      it "determines happy day dependencies for Visual Basic as well" $ do
        let inputRequirements =
              [r|
  <Project Sdk="Microsoft.NET.Sdk">
  <ItemGroup>
    <PackageReference Include="Microsoft.CodeAnalysis.FxCopAnalyzers" />
    <PackageReference Include="Microsoft.SourceLink.GitHub" PrivateAssets="All" />
    <PackageReference Include="Microsoft.Net.Compilers.Toolset" />
  </ItemGroup>
  </Project>
  |]
            input = VisualBasicNetProjectInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency VisualBasicNet (DependencyIdentifierNamed $ DependencyName "Microsoft.CodeAnalysis.FxCopAnalyzers") Nothing,
                    BasicDependency VisualBasicNet (DependencyIdentifierNamed $ DependencyName "Microsoft.SourceLink.GitHub") Nothing,
                    BasicDependency VisualBasicNet (DependencyIdentifierNamed $ DependencyName "Microsoft.Net.Compilers.Toolset") Nothing
                  ]

        result `shouldBe` expected
