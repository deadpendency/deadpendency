{-# LANGUAGE DataKinds #-}

module Common.Loader.SecretLoader
  ( loadSecret,
  )
where

import Data.ByteString.Base64.URL qualified as B64
import Network.Google qualified as G
import Network.Google.Auth.Scope qualified as G
import Network.Google.Resource.SecretManager.Projects.Secrets.Versions.Access qualified as G
import Network.Google.SecretManager.Types qualified as G

loadSecret ::
  ( G.AllowScopes s,
    G.HasScope' s SecretManagerRequiredScopes ~ 'True
  ) =>
  G.Env s ->
  Text ->
  IO ByteString
loadSecret env keyName = do
  putTextLn $ "Loading GitHub Key: " <> keyName

  let fullKeyName = "projects/dgtw-deadpendency-action-2/secrets/" <> keyName <> "/versions/latest"
      request = G.projectsSecretsVersionsAccess fullKeyName

  response <- (G.runResourceT . G.runGoogle env . G.send) request

  let encodedKeySecret =
        case response ^. (G.asvrPayload . _Just . G.spData) of
          Just theSecret -> theSecret
          Nothing -> error $ "Unexpected missing secret data for: " <> keyName

  let keySecret =
        case B64.decodeBase64 encodedKeySecret of
          Left decodeFailure -> error $ "Unexpected can't decode secret: " <> decodeFailure
          Right secret -> secret

  putTextLn $ "Loaded GitHub Key: " <> keyName

  pure keySecret

type SecretManagerRequiredScopes =
  '[ "https://www.googleapis.com/auth/cloud-platform"
   ]
