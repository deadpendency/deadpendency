module DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.PkgGoDev.PkgGoDevRepository
  ( determineSourceRepository,
  )
where

import Common.Model.Dependency.DependencyName
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.RepoHost
import Common.Model.Git.RepoName
import Common.Model.Git.RepoOwner
import Common.Parsing.Megaparsec
import Common.Parsing.NameParsing
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M

determineSourceRepository :: DependencyName -> Maybe QualifiedRepo
determineSourceRepository dependencyName =
  let dependencyNameText = dependencyName ^. #_ntText
   in mParseMaybe parsers dependencyNameText

parsers :: MParser QualifiedRepo
parsers =
  M.try parserGithubDep
    <|> M.try parserGoPkgIn
    <|> M.try parserK8s
    <|> M.try parserRSC
    <|> M.try parserUber
    <|> M.try parserGolang
    <|> M.try parserEtcd
    <|> parserGorm

skipVersion :: MParser ()
skipVersion = void $ M.optional $ M.char '.' *> M.some depNameChar

eosf :: MParser ()
eosf = void (M.char '/') <|> M.eof

-- github.com/Azure/go-autorest/autorest -> github.com/Azure/go-autorest
parserGithubDep :: MParser QualifiedRepo
parserGithubDep = do
  M.string "github.com/"
  repoOwner <- pack <$> M.someTill depNameChar (M.char '/')
  repoName <- pack <$> M.someTill depNameChar eosf
  pure $
    QualifiedRepo GitHub (RepoOwner repoOwner) (RepoName repoName)

-- gopkg.in/go-playground/validator.v9 -> github.com/go-playground/validator
-- gopkg.in/sourcemap.v1-> github.com/go-sourcemap/sourcemap
parserGoPkgIn :: MParser QualifiedRepo
parserGoPkgIn = do
  M.string "gopkg.in/"
  firstKey <- pack <$> M.someTill depNameChar (skipVersion *> eosf)
  maybeSecondKey <-
    M.optional $ pack <$> M.someTill depNameChar (skipVersion *> eosf)
  pure $
    case maybeSecondKey of
      Just secondKey -> QualifiedRepo GitHub (RepoOwner firstKey) (RepoName secondKey)
      Nothing -> QualifiedRepo GitHub (RepoOwner $ "go-" <> firstKey) (RepoName firstKey)

parserK8s :: MParser QualifiedRepo
parserK8s = M.try parserK8sBasic <|> parserK8sSigs

-- k8s.io/client-go -> github.com/kubernetes/client-go
parserK8sBasic :: MParser QualifiedRepo
parserK8sBasic = do
  M.string "k8s.io/"
  firstKey <- pack <$> M.someTill depNameChar (skipVersion *> M.eof)
  pure $
    QualifiedRepo GitHub (RepoOwner "kubernetes") (RepoName firstKey)

-- sigs.k8s.io/controller-runtime -> github.com/kubernetes-sigs/controller-runtime
parserK8sSigs :: MParser QualifiedRepo
parserK8sSigs = do
  M.string "sigs.k8s.io/"
  firstKey <- pack <$> M.someTill depNameChar (skipVersion *> M.eof)
  pure $
    QualifiedRepo GitHub (RepoOwner "kubernetes-sigs") (RepoName firstKey)

-- rsc.io/quote -> github.com/rsc/quote
-- rsc.io/quote/v3 -> github.com/rsc/quote
parserRSC :: MParser QualifiedRepo
parserRSC = do
  M.string "rsc.io/"
  firstKey <- pack <$> M.someTill depNameChar eosf
  pure $
    QualifiedRepo GitHub (RepoOwner "rsc") (RepoName firstKey)

-- go.uber.org/zap -> github.com/go-uber/zap
parserUber :: MParser QualifiedRepo
parserUber = do
  M.string "go.uber.org/"
  firstKey <- pack <$> M.someTill depNameChar eosf
  pure $
    QualifiedRepo GitHub (RepoOwner "go-uber") (RepoName firstKey)

-- golang.org/x/net/context -> github.com/golang/net
parserGolang :: MParser QualifiedRepo
parserGolang = do
  M.string "golang.org/x/"
  firstKey <- pack <$> M.someTill depNameChar eosf
  pure $
    QualifiedRepo GitHub (RepoOwner "golang") (RepoName firstKey)

-- go.etcd.io/bbolt -> github.com/etcd-io/bbolt
parserEtcd :: MParser QualifiedRepo
parserEtcd = do
  M.string "go.etcd.io/"
  firstKey <- pack <$> M.someTill depNameChar eosf
  pure $
    QualifiedRepo GitHub (RepoOwner "etcd-io") (RepoName firstKey)

parserGorm :: MParser QualifiedRepo
parserGorm = M.try parserGormBasic <|> parserGormDriver

-- gorm.io/gorm/logger -> github.com/go-gorm/gorm
parserGormBasic :: MParser QualifiedRepo
parserGormBasic = do
  M.string "gorm.io/gorm/"
  pure $
    QualifiedRepo GitHub (RepoOwner "go-gorm") (RepoName "gorm")

-- gorm.io/driver/mysql -> github.com/go-gorm/mysql
parserGormDriver :: MParser QualifiedRepo
parserGormDriver = do
  M.string "gorm.io/driver/"
  firstKey <- pack <$> M.someTill depNameChar (skipVersion *> M.eof)
  pure $
    QualifiedRepo GitHub (RepoOwner "go-gorm") (RepoName firstKey)
