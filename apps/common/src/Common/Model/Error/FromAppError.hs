module Common.Model.Error.FromAppError
  ( FromAppError (..),
  )
where

class FromAppError sharedError appError where
  fromAppError :: appError -> Maybe sharedError
