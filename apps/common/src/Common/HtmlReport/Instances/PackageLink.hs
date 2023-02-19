{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.HtmlReport.Instances.PackageLink
  ( renderPackageLink,
    renderPackageLinkWithName,
    parserRegistryAnchor,
  )
where

import Common.HtmlReport.HtmlReport
import Common.HtmlReport.HtmlReportDecodeError
import Common.Model.Dependency.DependencyName
import Common.Model.Ecosystem.Registry
import Common.Model.Report.PackageLink
import Common.Parsing.Megaparsec
import Common.Parsing.NameParsing
import Lucid
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M

instance ToHtmlReportBody PackageLink where
  toHtmlReportBody = renderPackageLink True "\x1F4E6"

renderPackageLinkWithName :: Bool -> PackageLink -> Html ()
renderPackageLinkWithName isTopLevel p@(PackageLink _ (DependencyName depNameText)) = renderPackageLink isTopLevel (toHtml depNameText) p

renderPackageLink :: Bool -> Html () -> PackageLink -> Html ()
renderPackageLink isTopLevel linkText (PackageLink registry dependencyName) =
  let dependencyNameText = dependencyName ^. #_ntText
      url =
        case registry of
          Npm -> "https://www.npmjs.com/package/" <> dependencyNameText
          Hackage -> "https://hackage.haskell.org/package/" <> dependencyNameText
          Packagist -> "https://packagist.org/packages/" <> dependencyNameText
          Pypi -> "https://pypi.org/project/" <> dependencyNameText
          RubyGems -> "https://rubygems.org/gems/" <> dependencyNameText
          Crates -> "https://crates.io/crates/" <> dependencyNameText
          NuGet -> "https://www.nuget.org/packages/" <> dependencyNameText
          PkgGoDev -> "https://pkg.go.dev/" <> dependencyNameText
          Maven -> "https://mvnrepository.com/artifact/" <> dependencyNameText
   in if isTopLevel
        then a_ [href_ url, title_ "registry package", id_ "package-repo-link"] linkText
        else a_ [href_ url] linkText

instance FromHtmlReportBody PackageLink where
  fromHtmlReportBody input = do
    let eitherResult = M.parse parserRegistryAnchor "PackageLink Report Body" input
    first (\e -> HtmlReportDecodeError $ "Failed to parse URL for package link hrefText: " <> input <> " error: " <> show @Text e) eitherResult

parserRegistryAnchor :: MParser PackageLink
parserRegistryAnchor = do
  M.skipManyTill M.anySingle (M.string "<a href=\"")
  packageLink <- parserRegistryLinkUrl
  M.skipManyTill M.anySingle (M.string "</a>")
  pure packageLink

parserRegistryLinkUrl :: MParser PackageLink
parserRegistryLinkUrl =
  M.try (parserRegistry Npm "https://www.npmjs.com/package/")
    <|> M.try (parserRegistry Hackage "https://hackage.haskell.org/package/")
    <|> M.try (parserRegistry Packagist "https://packagist.org/packages/")
    <|> M.try (parserRegistry Pypi "https://pypi.org/project/")
    <|> M.try (parserRegistry RubyGems "https://rubygems.org/gems/")
    <|> M.try (parserRegistry Crates "https://crates.io/crates/")
    <|> M.try (parserRegistry NuGet "https://www.nuget.org/packages/")
    <|> M.try (parserRegistry PkgGoDev "https://pkg.go.dev/")
    <|> parserRegistry Maven "https://mvnrepository.com/artifact/"

parserRegistry :: Registry -> Text -> MParser PackageLink
parserRegistry registry prefix = do
  M.string prefix
  dependencyName <- DependencyName . pack <$> M.some depNameExpandedChar
  pure $
    PackageLink registry dependencyName
