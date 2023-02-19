module Common.Model.Config.AppEnv
  ( AppEnv (..),
    appEnvAsText,
    appEnvFromText,
  )
where

data AppEnv
  = Prod
  | PreProd
  | Test
  deriving stock (Eq, Show, Generic)

appEnvAsText :: AppEnv -> Text
appEnvAsText =
  \case
    Prod -> "prod"
    PreProd -> "preprod"
    Test -> "test"

appEnvFromText :: Text -> Maybe AppEnv
appEnvFromText =
  \case
    "prod" -> Just Prod
    "preprod" -> Just PreProd
    "test" -> Just Test
    _ -> Nothing
