{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
import qualified Data.Text.Lazy.IO as R

import Text.Markdown
import Data.IORef
import Yesod

data App = App
    { homepageContent :: Html
    , visitorCount :: IORef Int
    }

mkYesod "App" [parseRoutes|
/ HomeR GET
|]
instance Yesod App

getHomeR :: Handler Html
getHomeR = do
    App {..} <- getYesod
    currentCount <- liftIO $ atomicModifyIORef visitorCount $ \i -> (i+1, i+1)
    defaultLayout $ doet)
        setTitle "Homepage"
        [whamlet|
            <article>#{homepageContent}
            <p>You are visitor number #{currentCount}
        |]

main :: IO ()
main = do
    rawMarkdown <- R.readFile "README.md"
    countRef <- newIORef 0
    warp 3000 App
        { homepageContent = markdown def rawMarkdown
        , visitorCount = countRef
        }
