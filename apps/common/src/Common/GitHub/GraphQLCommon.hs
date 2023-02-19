{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-deriving-strategies #-}

module Common.GitHub.GraphQLCommon
  ( executeGraphQL,
    executeGraphQL',
    asCommonError,
    asCommonMissingError,
    errorsContainsNotFound,
    GitObjectID (..),
    GitTimestamp (..),
    DateTime (..),
  )
where

import Common.Model.Error.CommonError
import Data.Morpheus.Client
import Data.Morpheus.Types.Internal.AST (GQLErrors, getCustomErrorType)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Network.HTTP.Req

declareGlobalTypesByName "../resource/minimal-github.graphql" ["DateTime", "GitTimestamp", "GitObjectID"]

executeGraphQL :: Text -> LByteString -> IO LByteString
executeGraphQL authToken payload = runReq defaultHttpConfig $ do
  let headers =
        header "Content-Type" "application/json"
          <> header "Authorization" ("token " <> encodeUtf8 authToken)
          <> header "Accept" "application/vnd.github+json"
          <> header "User-Agent" "deadpendency@deadpendency.com"
  responseBody
    <$> req
      POST
      (https "api.github.com" /: "graphql")
      (ReqBodyLbs payload)
      lbsResponse
      headers

executeGraphQL' :: Text -> LByteString -> IO LByteString
executeGraphQL' authToken payload = runReq defaultHttpConfig $ do
  let headers =
        header "Content-Type" "application/json"
          <> header "Authorization" ("token " <> encodeUtf8 authToken)
          <> header "Accept" "application/vnd.github+json"
          <> header "User-Agent" "deadpendency@deadpendency.com"
  responseBody
    <$> req
      POST
      (https "api.github.com" /: "graphql")
      (ReqBodyLbs payload)
      lbsResponse
      headers

-- this treats not found as exceptional
asCommonMissingError :: (Show a) => Either (FetchError a) a -> Either CommonError a
asCommonMissingError =
  \case
    Right a -> Right a
    Left (FetchErrorParseFailure parseFailure) -> Left $ GitHubResponseDecodeError $ show parseFailure
    Left FetchErrorNoResult -> Left $ GitHubInteractError "Unexpected no fetch result"
    Left (FetchErrorProducedErrors errors maybeA) ->
      if errorsContainsNotFound errors
        then Left $ GitHubUnexpectedNotFound $ "Errors: " <> show errors <> " Result: " <> show maybeA
        else Left $ GitHubInteractError $ "Errors: " <> show errors <> " Result: " <> show maybeA

asCommonError :: (Show a) => Either (FetchError a) a -> Either CommonError (Maybe a)
asCommonError =
  \case
    Right a -> Right $ Just a
    Left (FetchErrorParseFailure parseFailure) -> Left $ GitHubResponseDecodeError $ show parseFailure
    Left FetchErrorNoResult -> Left $ GitHubInteractError "Unexpected no fetch result"
    Left (FetchErrorProducedErrors errors maybeA) ->
      if errorsContainsNotFound errors
        then Right Nothing
        else Left $ GitHubInteractError $ "Errors: " <> show errors <> " Result: " <> show maybeA

errorsContainsNotFound :: GQLErrors -> Bool
errorsContainsNotFound = any (\e -> getCustomErrorType e == Just "NOT_FOUND")

newtype GitObjectID = GitObjectID
  { _gitObjectIDText :: Text
  }
  deriving stock (Eq, Show, Generic)

instance DecodeScalar GitObjectID where
  decodeScalar (String x) = Right $ GitObjectID x
  decodeScalar _ = Left "GitObjectId must be a String"

instance EncodeScalar GitObjectID where
  encodeScalar (GitObjectID value) = String value

newtype GitTimestamp = GitTimestamp
  { _gitTimestampTime :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

instance DecodeScalar GitTimestamp where
  decodeScalar (String x) = iso8601ParseM (unpack x) <&> GitTimestamp
  decodeScalar _ = Left "GitTimestamp must be a String"

instance EncodeScalar GitTimestamp where
  encodeScalar (GitTimestamp value) = String (pack $ iso8601Show value)

newtype DateTime = DateTime
  { _datetimeTime :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

instance DecodeScalar DateTime where
  decodeScalar (String x) = iso8601ParseM (unpack x) <&> DateTime
  decodeScalar _ = Left "DateTime must be a String"

instance EncodeScalar DateTime where
  encodeScalar (DateTime value) = String (pack $ iso8601Show value)
