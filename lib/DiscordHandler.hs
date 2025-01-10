{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DiscordHandler where

-- import Control.Error.Util
import Control.Exception
-- import Control.Monad
-- import Control.Monad.Catch
-- import Control.Monad.Except
import Control.Monad.IO.Class
import Crypto.Error
import Crypto.PubKey.Ed25519
import Data.Aeson
import Data.ByteString.Char8 qualified as BSB
-- import Data.ByteString qualified as BSW -- for ByteStringWord
import Data.Map                                     qualified as M
import GHC.Generics
import Network.DigitalOcean.CloudFunctions.Request  as Request
import Network.DigitalOcean.CloudFunctions.Response as Response
import System.Environment
-- import System.IO.Error
-- import Data.Aeson (Options(fieldLabelModifier))

data HttpException = HttpException {
    getCode    :: Int,
    getMessage :: String
}
    deriving stock (Show)
    deriving anyclass (Exception)

type DiscordWebhookPayloadType = Int

discordWebhookPayloadTypePing ∷ DiscordWebhookPayloadType
discordWebhookPayloadTypePing = 1

data DiscordEvent = DiscordEvent {
    _deType :: String,
    _deTimestamp :: String,
    _deData :: Object
} deriving (Eq, Show, Generic)

instance FromJSON DiscordEvent where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = drop 3 })

data DiscordWebhookRequest = DiscordWebhookRequest {
    _type :: DiscordWebhookPayloadType,
    _version :: Int,
    _application_id :: Integer,
    _event :: Maybe DiscordEvent
} deriving stock (Eq, Show, Generic)

instance FromJSON DiscordWebhookRequest where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = drop 1 })

newtype DiscordWebhookResponse = DiscordWebhookResponse {
    result :: String
} deriving stock (Eq, Show, Generic)

instance ToJSON DiscordWebhookResponse where
    toJSON = genericToJSON (defaultOptions { fieldLabelModifier = drop 1 })

-- | We want the string and also to decode it. No need to do any decoding for us today.
-- | TODO: the response should be short-circuitable


discordHandler ∷ (MonadIO m) ⇒ Request String → m (Response (Maybe DiscordWebhookResponse))
discordHandler Request { path = _path', Request.headers = headers', method = method', http = _http', args = args', ctx = _ctx' } = do
    -- don't overcomplicate it
    mPubKeyS <- liftIO $ lookupEnv "DISCORD_PUBLIC_KEY"

    -- TODO hex
    let mPubKey = maybeCryptoError . publicKey . BSB.pack =<< mPubKeyS

    case mPubKey of
        Nothing -> internalServerError "Missing/invalid public key"
        Just pubKey -> do
            let mReqData = decodeStrict (BSB.pack args') :: Maybe DiscordWebhookRequest
            case mReqData of
                Nothing -> badRequest "Invalid JSON"
                Just reqData ->
                    if _type reqData == discordWebhookPayloadTypePing && method' == "POST"
                    then
                        noContent
                    else do
                        -- TODO hex
                        let msSignature = maybeCryptoError . signature . BSB.pack =<< M.lookup "X-Signature-Ed25519" headers'
                        case msSignature of
                            Nothing -> badRequest "No signature"
                            Just signature' -> do
                                let msTimestamp = M.lookup "X-Signature-Timestamp" headers'
                                case msTimestamp of
                                    Nothing -> badRequest "No timestamp"
                                    Just timestamp -> do
                                        let signedMessage = BSB.pack $ timestamp <> args' -- TODO get POST data and add it to timestamp
                                        let verified = verify pubKey signedMessage signature'
                                        if verified
                                            then ok "OK"
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