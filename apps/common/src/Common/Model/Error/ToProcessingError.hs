module Common.Model.Error.ToProcessingError
  ( ToProcessingError (..),
  )
where

import Common.Model.Error.ProcessingError

class ToProcessingError sharedError where
  toProcessingError :: sharedError -> Maybe ProcessingError
