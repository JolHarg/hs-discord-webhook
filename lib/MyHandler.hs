{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MyHandler where

-- import Control.Error.Util
import Control.Exception
import Control.Monad
-- import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.IO.Class
import Crypto.Sign.Ed25519
import Data.Aeson
import Data.ByteString.Char8 qualified as BSB
import Data.ByteString qualified as BSW -- for ByteStringWord
import Data.Map                                     qualified as M
import GHC.Generics
import Network.DigitalOcean.CloudFunctions.Request  as Request
import Network.DigitalOcean.CloudFunctions.Response as Response
import System.Environment
import System.IO.Error
import Data.Aeson (Options(fieldLabelModifier))

data HttpException = HttpException {
    getCode    :: Int,
    getMessage :: String
}
    deriving stock (Show)
    deriving anyclass (Exception)

type DiscordWebhookPayloadType = Int

discordWebhookPayloadTypePing ∷ DiscordWebhookPayloadType
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
myHandler Request { path = _path', Request.headers = headers', method = method', http = _http', args = args', ctx = _ctx' } = do
    -- don't overcomplicate it
    mPubKey <- PublicKey . BSW.pack <$> lookupEnv "DISCORD_PUBLIC_KEY"

    case mPubKey of
        Nothing -> internalServerError "Missing public key"
        Just pubKey -> do
            let mReqData = decodeStrict args' :: Maybe DiscordWebhookRequest
            case mReqData of
                Nothing -> badRequest "Invalid JSON"
                Just reqData ->
                    if _type reqData == discordWebhookPayloadTypePing && method' == "POST"
                    then
                        noContent
                    else do
                        let msSignature = Signature . BSW.pack <$> M.lookup "X-Signature-Ed25519" headers'
                        case msSignature of
                            Nothing -> badRequest "No signature"
                            Just signature -> do
                                let msTimestamp = BSW.pack <$> M.lookup "X-Signature-Timestamp" headers'
                                case msTimestamp of
                                    Nothing -> badRequest "No timestamp"
                                    Just timestamp -> do
                                        let verify = dverify pubKey signedMessage signature
                                        if verify
                                            then ok
                                            else forbidden "Invalid signature"
    where
        response status resp = pure $ Response {
            body = Just (DiscordWebhookResponse resp),
            statusCode = status,
            Response.headers = [
                ("Content-Type", "application/json")
                ]
        }
        internalServerError = response 500
        badRequest = response 400
        forbidden = response 401
        ok = response 200
        noContent = pure $ Response {
            body = Nothing,
            statusCode = 204,
            Response.headers = [
            ]
        }