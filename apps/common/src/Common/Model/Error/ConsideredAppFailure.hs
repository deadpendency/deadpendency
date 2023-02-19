module Common.Model.Error.ConsideredAppFailure
  ( ConsideredAppFailure (..),
  )
where

class ConsideredAppFailure e where
  consideredAppFailure :: e -> Bool
