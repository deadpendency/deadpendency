module Common.Parsing.Xml
  ( stripCDataSections,
  )
where

import Data.ByteString qualified as BS
import Data.ByteString.Search qualified as BSS

-- This is needed because hexml doesn't support CDATA https://github.com/ndmitchell/hexml/issues/1
{-
asdf<![CDATA[Copyright 1999-2017]]>ghjk
-}
stripCDataSections :: ByteString -> ByteString
stripCDataSections input =
  -- the majority won't have this, so presumably this is overall faster
  -- could also use `indicies` from BSS, but the package is semi dead so want to avoid it as much as possible
  if BS.isInfixOf "<![CDATA[" input
    then -- easier to just convert them to comments than attempt to strip them

      ( fromLazy
          . BSS.replace @ByteString "]]>" "-->"
          . fromLazy
          . BSS.replace @ByteString "<![CDATA[" "<!--"
          . fromLazy
          . BSS.replace @ByteString "<!-- Global site tag (gtag.js) - Google Analytics -->" ""
          -- horible hack to avoid issue, ideally want to strip between the CDATA sections but this is painful. Will probably switch to xeno at some point
          -- https://repo1.maven.org/maven2/net/ttddyy/datasource-assert/1.0/datasource-assert-1.0.pom
      )
        input
    else input
