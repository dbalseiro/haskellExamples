{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
    ( Application
    , Response
    , responseLBS
    )
import Network.Wai.Conduit
    ( sourceRequestBody
    )
import Network.Wai.Handler.Warp
    ( run
    )
import Network.HTTP.Types
    ( status200
    , status400
    )
import Control.Exception
    ( handle
    , SomeException
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Data.Aeson
    ( encode
    , object
    , (.=)
    , Value
    )
import Data.Aeson.Parser
    ( json
    )
import Data.Conduit
    ( ($$)
    )
import Data.Conduit.Attoparsec
    ( sinkParser
    )

main :: IO ()
main = run 3000 app

app :: Application
app req sendResponse = handle (sendResponse . invalidJson) $ do
    value <- sourceRequestBody req $$ sinkParser json
    newValue <- liftIO $ modValue value
    sendResponse $ responseLBS
        status200
        [("Content-Type", "application/json")]
        $ encode newValue

invalidJson :: SomeException -> Response
invalidJson ex = responseLBS
    status400
    [("Content-Type", "application/json")]
    $ encode $ object [ "message" .= show ex ]

modValue :: Value -> IO Value
modValue v = return $ object [ "value" .= v ]
