module CommonTest.Gen.General
  ( genPositiveInt,
    genAlphaText,
    genUTCTime,
    genNonEmptyVector,
    genVector,
    genThese,
    genURI,
  )
where

import Data.Vector qualified as V
import Data.Vector.NonEmpty qualified as NV
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Relude.Unsafe (fromJust)
import Text.URI (URI)
import Text.URI qualified as URI

genNonEmptyVector :: (MonadGen m) => Range Int -> m a -> m (NV.NonEmptyVector a)
genNonEmptyVector range ma = NV.fromNonEmpty <$> Gen.nonEmpty range ma

genVector :: (MonadGen m) => Range Int -> m a -> m (V.Vector a)
genVector range ma = V.fromList <$> Gen.list range ma

genPositiveInt :: Gen Int
genPositiveInt = Gen.int (Range.constant 1 maxBound)

genAlphaText :: Gen Text
genAlphaText = Gen.text (Range.constant 1 10) Gen.alpha

-- waiting on https://github.com/hedgehogqa/haskell-hedgehog/issues/215
genUTCTime :: (MonadGen m) => m UTCTime
genUTCTime = do
  y <- toInteger <$> Gen.int (Range.constant 2000 2019)
  m <- Gen.int (Range.constant 1 12)
  d <- Gen.int (Range.constant 1 28)
  let day = fromGregorian y m d
  secs <- toInteger <$> Gen.int (Range.constant 0 86400)
  let diff' = secondsToDiffTime secs
  pure $ UTCTime day diff'

genThese :: Gen a -> Gen b -> Gen (These a b)
genThese genA genB =
  Gen.choice
    [ genA <&> This,
      genB <&> That,
      These <$> genA <*> genB
    ]

genURI :: Gen URI
genURI = Gen.constant $ fromJust $ URI.mkURI "https://darcs.net/"
