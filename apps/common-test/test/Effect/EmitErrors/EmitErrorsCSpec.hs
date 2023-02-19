{-# OPTIONS_GHC -fno-warn-orphans #-}

module Effect.EmitErrors.EmitErrorsCSpec (spec) where

import Common.Effect.AppEventEmit.Carrier.AppEventEmitRetC
import Common.Effect.AppEventEmit.Model.AppEvent
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventLevel
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Effect.EmitErrors.Carrier.EmitErrorsC
import Common.Model.Error.ConsideredAppFailure
import Control.Carrier.Error.Either
import Test.Hspec

spec :: Spec
spec = parallel $
  context "When emitting errors" $ do
    it "no errors produces a pass" $ do
      let handler =
            runEmitErrorsC @Text
              >>> runError @Text
              >>> runAppEventEmitRet
              >>> runIdentity

          input = liftEither (Right 1 :: Either Text Int)
          (appEvents, eitherResult) = handler input

      appEvents `shouldBe` []
      eitherResult `shouldBe` Right 1

    it "an error produces a log + left" $ do
      let handler =
            runEmitErrorsC @Text
              >>> runError @Text
              >>> runAppEventEmitRet
              >>> runIdentity

          errorText = "Woah"
          input = liftEither (Left errorText :: Either Text Int)
          (appEvents, eitherResult) = handler input

      appEvents `shouldBe` [AppEvent AppEventLevelError (AppEventMessage "Request Error") (Just $ AppEventAdditional errorText)]
      eitherResult `shouldBe` Left "Woah"

    it "a warning produces a log + left" $ do
      let handler =
            runEmitErrorsC @Text
              >>> runError @Text
              >>> runAppEventEmitRet
              >>> runIdentity

          errorText = "WARN"
          input = liftEither (Left errorText :: Either Text Int)
          (appEvents, eitherResult) = handler input

      appEvents `shouldBe` [AppEvent AppEventLevelWarning (AppEventMessage "Request Warning") (Just $ AppEventAdditional errorText)]
      eitherResult `shouldBe` Left "WARN"

instance ConsideredAppFailure Text where
  consideredAppFailure =
    \case
      "WARN" -> False
      _ -> True
