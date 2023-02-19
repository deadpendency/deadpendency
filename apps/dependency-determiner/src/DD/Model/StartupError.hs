module DD.Model.StartupError
  ( StartupError (..),
  )
where

data StartupError
  = ConfigLoadError
  | GitHubKeyLoadError
  deriving stock (Show)
