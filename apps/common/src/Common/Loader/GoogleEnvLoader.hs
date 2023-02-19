module Common.Loader.GoogleEnvLoader (loadGoogleEnv) where

import Network.Google qualified as G
import Network.HTTP.Client (Manager)

loadGoogleEnv :: (G.AllowScopes s) => Manager -> IO (G.Env s)
loadGoogleEnv manager = do
  putTextLn "Loading Google Env"
  logger <- G.newLogger G.Info stdout
  credentials <- G.getApplicationDefault manager
  googleEnv <- G.newEnvWith credentials logger manager
  putTextLn "Loaded Google Env"
  pure googleEnv
