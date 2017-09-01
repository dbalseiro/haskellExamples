{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Data.Text          (Text, pack)
import           System.Environment
import           Yesod

data App = App
    { myApproot      :: Text
    , welcomeMessage :: Text
    }

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App where
    approot = ApprootMaster myApproot

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    App {..} <- getYesod
    setTitle "Environment variables"
    [whamlet|
        <p>Here's the welcome message: #{welcomeMessage}
        <p>
            <a href=@{HomeR}>And a link to: @{HomeR}
    |]

main :: IO ()
main = do
    myApproot <- pack <$> getEnv "APPROOT"
    welcomeMessage <- pack <$> getEnv "WELCOME_MESSAGE"
    warp 3000 App {..}
