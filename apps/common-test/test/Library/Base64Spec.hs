module Library.Base64Spec (spec) where

import CommonTest.Gen.Model.InterchangeEvent
import Data.Aeson
import Data.ByteString.Base64 qualified as B64
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = parallel $
  modifyMaxSuccess (const 1000) $
    xcontext "base64 sanity checks" $ do
      it "report generator interchange event should roundtrip" $
        hedgehog $ do
          dfEvent <- forAll genDependenciesFetched

          let encoded = B64.encodeBase64' $ fromLazy $ encode dfEvent

          let eitherDfEvent = B64.decodeBase64 encoded >>= (first pack . eitherDecodeStrict')

          Right dfEvent === eitherDfEvent

      it "test random base64 decode" $ do
        let result = void $ B64.decodeBase64 testInput

        result `shouldBe` Right ()

-- it "test random base64 encode / decode" $ do
--   let eitherIE = first pack $ eitherDecodeStrict' @(InterchangeEvent RunResult) testInput
--       eitherEncoded = eitherIE <&> (B64.encodeBase64' . fromLazy . encode)
--       eitherResult = eitherEncoded >>= (void . B64.decodeBase64)

--   eitherResult `shouldBe` Right ()

--   encoded `shouldBe` expectedBS

testInput :: ByteString
testInput = ""
