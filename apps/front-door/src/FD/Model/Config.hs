module FD.Model.Config
  ( Config (..),
  )
where

newtype Config = Config
  { _githubWebhookSecretName :: Text
  }
  deriving stock (Generic)
