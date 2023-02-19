module Common.Loader.InstanceConfigLoader (loadInstanceConfig) where

import Common.Model.Config.InstanceConfig
import Network.Google.Compute.Metadata qualified as G
import Network.HTTP.Client (Manager)
import System.IO.Error (ioError, userError)

loadInstanceConfig :: Manager -> IO InstanceConfig
loadInstanceConfig manager = do
  putTextLn "Loading Instance Config"
  projectId <- G.getProjectId manager
  zone <- G.getZone manager
  instanceConfig <-
    case rawZoneToRegion zone of
      Just region ->
        pure
          InstanceConfig
            { _projectId = projectId,
              _instanceRegion = region
            }
      Nothing -> ioError $ userError ("Error - Instance zone in unexpected format: " <> unpack zone)
  putTextLn $ "Instance Config Loaded: " <> show instanceConfig
  pure instanceConfig

-- projects/PROJECT-NUMBER/zones/REGION
rawZoneToRegion :: Text -> Maybe Text
rawZoneToRegion = viaNonEmpty last . splitOn "/"
