module Common.Model.Config.InstanceConfig
  ( InstanceConfig (..),
  )
where

data InstanceConfig = InstanceConfig
  { _projectId :: Text,
    _instanceRegion :: Text
  }
  deriving stock (Show, Generic)
