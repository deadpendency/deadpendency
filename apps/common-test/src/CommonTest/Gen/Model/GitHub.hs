{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module CommonTest.Gen.Model.GitHub where

import Common.Model.GitHub.Auth.GHInstallationAuth
import Common.Model.GitHub.GHAppInstallationId
import Common.Model.GitHub.GHNodeId
import Common.Model.GitHub.GHRepoFullName
import Common.Model.GitHub.GHRepoOwnerType
import Common.Model.GitHub.GHUserName
import CommonTest.Gen.General
import Hedgehog
import Hedgehog.Gen qualified as Gen

genGHNodeId :: Gen GHNodeId
genGHNodeId = GHNodeId <$> genAlphaText

genGHUserName :: Gen GHUserName
genGHUserName = GHUserName <$> genAlphaText

genGHRepoOwnerType :: Gen GHRepoOwnerType
genGHRepoOwnerType = Gen.enumBounded

genGHAppInstallationId :: Gen GHAppInstallationId
genGHAppInstallationId = GHAppInstallationId <$> genPositiveInt

genGHInstallationAuth :: Gen GHInstallationAuth
genGHInstallationAuth = do
  installId <- genGHAppInstallationId
  token <- genAlphaText
  expirationType <- genUTCTime
  pure
    GHInstallationAuth
      { _installationId = installId,
        _token = token,
        _expirationTime = expirationType
      }

genGHRepoFullName :: Gen GHRepoFullName
genGHRepoFullName = GHRepoFullName <$> genAlphaText
