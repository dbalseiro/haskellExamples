{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Conduit
    ( withManager
    , parseUrl
    , method
    , http
    , requestBody
    , responseBody
    , RequestBody (RequestBodyLBS)
    )

import Control.Monad.IO.Class
    ( liftIO
    )

import Data.Aeson
    ( Value (Object, String)
    , encode
    , object
    , json
    , (.=)
    )

import Data.Conduit
    ( ($$+-)
    )

import Data.Conduit.Attoparsec
    ( sinkParser
    )

main :: IO ()
main = withManager $ \manager -> do
    value <- liftIO makeValue
    -- we need to know the size of the request body so
    -- we convert to bytestring
    let valueBS = encode value
    req' <- liftIO $ parseUrl "http://localhost:3000/"
    let req = req' { method = "POST", requestBody = RequestBodyLBS valueBS }
    res <- http req manager
    resValue <- responseBody res $$+- sinkParser json
    liftIO $ handleResponse resValue

makeValue :: IO Value
makeValue = return $ object
    [ "foo" .= ("bar" :: String) ]

handleResponse :: Value -> IO ()
handleResponse = print
