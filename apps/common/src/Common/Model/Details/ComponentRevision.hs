module Common.Model.Details.ComponentRevision
  ( ComponentRevision (..),
  )
where

newtype ComponentRevision = ComponentRevision
  { _componentRevision :: Text
  }
  deriving stock (Eq, Show, Generic)
