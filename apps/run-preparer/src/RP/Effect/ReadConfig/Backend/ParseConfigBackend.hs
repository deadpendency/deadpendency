module RP.Effect.ReadConfig.Backend.ParseConfigBackend
  ( parseConfig,
  )
where

import Common.Model.RepoConfig.RepoConfig
import Data.YAML
import RP.Effect.ReadConfig.Backend.RepoConfig ()
import RP.Effect.ReadConfig.Model.ReadConfigError

parseConfig :: Text -> Either ReadConfigError RepoConfig
parseConfig = first (ParseConfigFailed . pack . snd) . decode1 . encodeUtf8
