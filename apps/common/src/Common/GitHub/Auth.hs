module Common.GitHub.Auth
  ( generateInstallationAuth,
    generateSharedGHAppAuth,
    loadAuthPrereqs,
  )
where

import Common.GitHub.Internal.InstallationAuthResponse
import Common.Model.GitHub.Auth.GHAppAuthPrereqs
import Common.Model.GitHub.Auth.GHInstallationAuth
import Common.Model.GitHub.Auth.GHSharedAppAuth
import Common.Model.GitHub.GHAppId
import Common.Model.GitHub.GHAppInstallationId
import Common.Model.GitHub.GHAppRawPrivateKey
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.X509 (PrivKey (PrivKeyRSA))
import Data.X509.Memory (readKeyFileFromMemory)
import Network.HTTP.Req
import Web.JWT qualified as JWT

generateInstallationAuth :: GHAppInstallationId -> GHSharedAppAuth -> IO GHInstallationAuth
generateInstallationAuth ghAppInstallationId sharedAppAuth = do
  let installId = ghAppInstallationId ^. #_ntInt
      jwtAppAuthToken = sharedAppAuth ^. #_jwtToken

      headers =
        header "Content-Type" "application/json"
          <> header "Authorization" ("Bearer " <> encodeUtf8 jwtAppAuthToken)
          <> header "Accept" "application/vnd.github+json"
          <> header "User-Agent" "deadpendency@deadpendency.com"

  (InstallationAuthResponse newToken newExpiresAt) <-
    runReq defaultHttpConfig $
      responseBody
        <$> req
          POST
          (https "api.github.com" /: "app" /: "installations" /: show @Text installId /: "access_tokens")
          NoReqBody
          jsonResponse
          headers

  pure
    GHInstallationAuth
      { _installationId = ghAppInstallationId,
        _token = newToken,
        _expirationTime = newExpiresAt
      }

generateSharedGHAppAuth :: GHAppAuthPrereqs -> IO (Either Text GHSharedAppAuth)
generateSharedGHAppAuth prereqs = do
  let appId = prereqs ^. (#_appId . #_ntInt)
      privateKey = prereqs ^. #_privateKey
  currentTime <- getCurrentTime
  -- 10 minutes in the future is the maximum
  -- Github seems unhappy with exactly 10 minutes, so we go 1 second less
  let expiryTime = addUTCTime ((10 * 60) - 1) currentTime
      maybeJWTClaims = do
        currentTime' <- maybeToRight "Error cannot load jwt current time" $ JWT.numericDate (utcTimeToPOSIXSeconds currentTime)
        expiryTime' <- maybeToRight "Error cannot load jwt expiry time" $ JWT.numericDate (utcTimeToPOSIXSeconds expiryTime)
        issuer' <- maybeToRight "Error cannot load app expiry time" $ JWT.stringOrURI (show appId)
        pure
          mempty
            { JWT.iat = Just currentTime',
              JWT.exp = Just expiryTime',
              JWT.iss = Just issuer'
            }
      result =
        maybeJWTClaims
          <&> \claims ->
            GHSharedAppAuth
              { _jwtToken = JWT.encodeSigned (JWT.EncodeRSAPrivateKey privateKey) mempty claims,
                _expiryTime = expiryTime
              }
  pure result

loadAuthPrereqs :: GHAppId -> GHAppRawPrivateKey -> Either Text GHAppAuthPrereqs
loadAuthPrereqs appId privateKey =
  let privKeys = readKeyFileFromMemory $ privateKey ^. #_ntByteString
   in case privKeys of
        [PrivKeyRSA rsaKey] ->
          Right
            GHAppAuthPrereqs
              { _appId = appId,
                _privateKey = rsaKey
              }
        _ -> Left "Unexpected key from file"
