module Common.Parsing.Megaparsec
  ( MParser,
    mParseMaybe,
    -- mParseMaybeTraced
    -- maybeToFail,
    spaceTab,
  )
where

import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M

type MParser = M.Parsec Void Text

-- used to avoid the auto parsing of eof by the official parseMaybe
-- https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec.html#v:parseMaybe
mParseMaybe :: M.Parsec e s a -> s -> Maybe a
mParseMaybe p = rightToMaybe . M.parse p ""

-- mParseMaybeTraced :: (Show s, Show (M.Token s), Show e, Show a) => M.Parsec e s a -> s -> Maybe a
-- mParseMaybeTraced p = rightToMaybe . (\a -> traceShow a a) . M.parse p ""

spaceTab :: MParser ()
spaceTab = M.hspace <|> void M.tab

-- maybeToFail :: MonadFail m => String -> Maybe a -> m a
-- maybeToFail errorAsText = maybe (fail errorAsText) pure
