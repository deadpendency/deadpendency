module Common.Model.Error.ToAppError
  ( ToAppError (..),
  )
where

class ToAppError sharedError appError where
  toAppError :: sharedError -> appError
