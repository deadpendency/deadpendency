{-# LANGUAGE AllowAmbiguousTypes #-}

module Common.Handler
  ( cleanGoogleTraceId,
    handleError,
    handleErrorWithAdditional,
  )
where

import Common.Effect.PublishComponentResult.Model.ComponentResult
import Common.Model.Error.ToProcessingError
import Control.Algebra (Has)
import Control.Effect.Catch (Catch, catchError)
import Data.Text qualified as T

handleError ::
  forall e sig m b a.
  ( Has (Catch e) sig m,
    ToProcessingError e
  ) =>
  m a ->
  (a -> m (ComponentResult b)) ->
  m (ComponentResult b)
handleError ma f =
  let mComponentResultB = ma >>= f
   in catchError @e
        mComponentResultB
        ( \e ->
            case toProcessingError e of
              Just failure -> pure $ FailureComponentResult failure
              Nothing -> mComponentResultB
        )

handleErrorWithAdditional ::
  forall e sig m b a.
  ( Has (Catch e) sig m,
    ToProcessingError e
  ) =>
  m a ->
  (a -> m (ComponentResult b)) ->
  (e -> m ()) ->
  m (ComponentResult b)
handleErrorWithAdditional ma f additionalErrorHandling =
  let mComponentResultB = ma >>= f
   in catchError @e
        mComponentResultB
        ( \e ->
            case toProcessingError e of
              Just failure -> additionalErrorHandling e $> FailureComponentResult failure
              Nothing -> additionalErrorHandling e *> mComponentResultB
        )

cleanGoogleTraceId :: T.Text -> T.Text
cleanGoogleTraceId = T.takeWhile (not . (==) '/')
