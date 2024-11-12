{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE OverloadedLists #-}

module MyHandler where

-- import Control.Error.Util
import Control.Exception
-- import Control.Monad
-- import Control.Monad.Catch
-- import Control.Monad.Except
import Control.Monad.IO.Class
-- import Crypto.Sign.Ed25519
import Data.Aeson
-- import Data.ByteString.Char8 qualified as BSB
-- import Data.ByteString qualified as BSW -- for ByteStringWord
-- import Data.Map                                     qualified as M
import GHC.Generics
import Network.DigitalOcean.CloudFunctions.Request  as Request
import Network.DigitalOcean.CloudFunctions.Response as Response
-- import System.Environment
-- import System.IO.Error
-- import Data.Aeson (Options(fieldLabelModifier))

data HttpException = HttpException {
    getCode    :: Int,
    getMessage :: String
}
    deriving stock (Show)
    deriving anyclass (Exception)

type DiscordWebhookPayloadType = Int

discordWebhookPayloadTypePing :: DiscordWebhookPayloadType
discordWebhookPayloadTypePing = 1

newtype DiscordWebhookRequest = DiscordWebhookRequest {
    _type :: DiscordWebhookPayloadType
} deriving stock (Eq, Show, Generic)

instance FromJSON DiscordWebhookRequest where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = drop 1 })

newtype DiscordWebhookResponse = DiscordWebhookResponse {
    _RENAME_ME_type :: DiscordWebhookPayloadType
} deriving stock (Eq, Show, Generic)

instance ToJSON DiscordWebhookResponse where
    toJSON = genericToJSON (defaultOptions { fieldLabelModifier = drop 1 })

-- | We want the string and also to decode it. No need to do any decoding for us today.
-- | TODO: the response should be short-circuitable


myHandler ∷ (MonadIO m) ⇒ Request String → m (Response (Maybe DiscordWebhookResponse))
myHandler Request { path = _path', Request.headers = _headers', method = _method', http = _http', args = _args', ctx = _ctx' } = do
    -- don't overcomplicate it
    -- TODO short circuiting afterwards

    pure $ Response {
        body = Just (DiscordWebhookResponse discordWebhookPayloadTypePing),
        statusCode = 200,
        Response.headers = [
            ("Content-Type", "application/json")
            ]
    }
{-}

    mPubKey <- PublicKey . BSW.pack <$> lookupEnv "DISCORD_PUBLIC_KEY"

    case mPubKey of
        Nothing -> pure $ Response {
            body = Just DiscordWebhookResponse "Missing public key",
            statusCode = 500,
            Response.headers = [
                ("Content-Type", "application/json")
                ]
        }
        Just pubKey -> do

            let mReqData = decodeStrict args' :: Maybe DiscordWebhookRequest

            case mReqData of
                Nothing -> pure $ Response {
                    body = Just DiscordWebhookResponse "Invalid JSON",
                    statusCode = 400,
                    Response.headers = [
                        ("Content-Type", "application/json")
                        ]
                }
                Just reqData ->
                    if _type reqData == discordWebhookRequestTypePing && method' == "POST"
                    then
                        pure $ Response {
                            body = Nothing,
                            statusCode = 204,
                            Response.headers = [
                            ]
                        }
                    else
                        let
                            msSignature = Signature . BSW.pack <$> M.lookup "X-Signature-Ed25519" headers'

                            msTimestamp = BSW.pack <$> M.lookup "X-Signature-Timestamp" headers'
                        in
                            pure $ Response {
                                body = Just DiscordWebhookResponse "OK",
                                statusCode = 200,
                                Response.headers = [
                                        ("Content-Type", "application/json")
                                        ]
                            }
-}
    {-result <- runExceptT $ do
        signature <- ExceptT . pure . note (HttpException 400 "No signature found") $ Signature . BSW.pack <$> M.lookup "X-Signature-Ed25519" headers'
        timestamp <- ExceptT . pure . note (HttpException 400 "No timestamp found") $ BSW.pack <$> M.lookup "X-Signature-Timestamp" headers'
        publicKey <- ExceptT . pure . note (HttpException 500 "No public key found") $ PublicKey mPubKey
        when . not (dverify publicKey signedMessage signature) . throwError $ HttpException 401 "Invalid signature"
        let rawBody = encode args'
        pure "Hello World!"
    case result of
        Left ex -> pure $ Response {
            body = getMessage ex,
            statusCode = getCode ex,
            Response.headers = [
                ("Content-Type", "application/json")
                ]
        }
        Right msg -> pure $ Response {
            body = msg,
            statusCode = 200,
            Response.headers = [
                ("Content-Type", "application/json")
                ]
        }
        -}
