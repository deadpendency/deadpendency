module Common.Loader.HttpManagerLoader (loadHttpManager) where

import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

loadHttpManager :: IO Manager
loadHttpManager = do
  putTextLn "Loading HTTP Manager"
  manager <- newManager tlsManagerSettings
  putTextLn "HTTP Manager Loaded"
  pure manager
