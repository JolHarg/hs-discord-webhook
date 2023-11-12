{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLists    #-}

module MyHandler where

import Control.Error.Util
import Control.Exception
import Control.Monad.Except
import Control.Monad.IO.Class
import Crypto.Sign.Ed25519
import Data.Aeson
import Data.ByteString.Char8
import Data.Map                                     qualified as M
import GHC.Generics
import Network.DigitalOcean.CloudFunctions.Request  as Request
import Network.DigitalOcean.CloudFunctions.Response as Response
import System.Environment

data MyData = MyData {
    myString :: String,
    myInt    :: Int
}
    deriving stock Generic
    deriving anyclass (FromJSON, ToJSON)

data HttpException = HttpException {
    getCode    :: Int,
    getMessage :: String
}
    deriving stock (Show)
    deriving anyclass (Exception)


myHandler ∷ MonadIO m ⇒ Request Object → m (Response String)
myHandler Request { path = _path', Request.headers = headers', method = _method', http = _http', args = args', ctx = _ctx' } = do
    result <- runExceptT $ do
        eSignature <- ExceptT . pure . note (HttpException 400 "No signature found") $ M.lookup "X-Signature-Ed25519" headers'
        eTimestamp <- ExceptT . pure . note (HttpException 400 "No timestamp found") $ M.lookup "X-Signature-Timestamp" headers'
        publicKey <- liftIO $ getEnv "DISCORD_PUBLIC_KEY"
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
