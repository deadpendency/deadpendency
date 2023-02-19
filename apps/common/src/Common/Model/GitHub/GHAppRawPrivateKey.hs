module Common.Model.GitHub.GHAppRawPrivateKey
  ( GHAppRawPrivateKey (..),
  )
where

newtype GHAppRawPrivateKey = GHAppRawPrivateKey
  { _ntByteString :: ByteString
  }
  deriving stock (Eq, Show, Generic)
