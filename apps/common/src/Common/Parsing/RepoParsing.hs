module Common.Parsing.RepoParsing
  ( parserQualifiedRepo,
    parserCoreQualifiedRepo,
    repoNameChar,
    parserRepoHost,
    parserSimpleRepo,
    parserRepo,
    parserRepoUnknown,
    parserCustomQRRepo,
  )
where

import Common.Model.Git.QualifiedRepo
import Common.Model.Git.Repo
import Common.Model.Git.RepoHost
import Common.Model.Git.RepoName
import Common.Model.Git.RepoOwner
import Common.Parsing.Megaparsec
import Common.Parsing.NameParsing
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.URI (URI)
import Text.URI qualified as URI

-- mostly allowing for examples from https://docs.npmjs.com/files/package.json#git-urls-as-dependencies

parserRepo :: MParser Repo
parserRepo = (RepoQR <$> M.try parserQualifiedRepo) <|> parserRepoUnknown

parserRepoUnknown :: MParser Repo
parserRepoUnknown = do
  uri <- URI.parser
  if uriIsValid uri
    then pure $ RepoUnknown uri
    else M.fancyFailure (one . M.ErrorFail $ "Invalid URI: " <> show @String uri)

-- ie. URIs that are valid as a git location
uriIsValid :: URI -> Bool
uriIsValid uri =
  uri /= URI.emptyURI
    && URI.isPathAbsolute uri

parserCustomQRRepo :: MParser QualifiedRepo -> MParser Repo
parserCustomQRRepo qrRepoParser = (RepoQR <$> qrRepoParser) <|> (RepoUnknown <$> URI.parser)

-- python needs to use #egg=blah so we expose a version that won't consume post #readme etc.
parserCoreQualifiedRepo :: MParser QualifiedRepo
parserCoreQualifiedRepo = do
  M.optional (M.string "git+")
  void (M.string "git") <|> void (M.string "ssh") <|> void (M.string "http" *> M.optional (M.char 's'))
  M.optional $ M.string "://"
  M.optional $ M.try $ M.manyTill depNameChar (M.char '@')
  M.optional (M.string "www.")
  repoHost <- parserRepoHost
  M.char '/' <|> M.char ':'
  repoOwner <- RepoOwner . pack <$> M.some orgNameChar
  M.char '/'
  repoName <- RepoName . pack <$> (M.try (M.someTill repoNameChar parserGitPost) <|> M.some repoNameChar)
  M.optional $ M.try $ parserTagPost <|> parserSubLinkPost
  pure $
    QualifiedRepo repoHost repoOwner repoName

parserQualifiedRepo :: MParser QualifiedRepo
parserQualifiedRepo = parserCoreQualifiedRepo <* M.try (M.optional parserHashPost)

parserSubLinkPost :: MParser ()
parserSubLinkPost = do
  M.char '/'
  M.many depNameChar
  pure ()

parserGitPost :: MParser ()
parserGitPost = M.string ".git" $> ()

parserTagPost :: MParser ()
parserTagPost = do
  M.char '@'
  M.some repoNameChar
  pure ()

parserHashPost :: MParser ()
parserHashPost = do
  M.char '#'
  M.many repoNameChar
  pure ()

parserSimpleRepo :: MParser QualifiedRepo
parserSimpleRepo = do
  repoOwner <- RepoOwner . pack <$> M.someTill orgNameChar (M.char '/')
  repoName <- RepoName . pack <$> M.some repoNameChar
  M.optional $ M.char '/'
  pure $
    QualifiedRepo GitHub repoOwner repoName

parserRepoHost :: MParser RepoHost
parserRepoHost =
  (M.string "github.com" $> GitHub)
    <|> (M.string "bitbucket.org" $> Bitbucket)
    <|> (M.string "gitlab.com" $> GitLab)

orgNameChar :: MParser Char
orgNameChar = depNameChar

repoNameChar :: MParser Char
repoNameChar = depNameChar <|> M.char '.'
