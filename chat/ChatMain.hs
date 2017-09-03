{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
module ChatMain where

import           Chat
import           Chat.Data
import           Control.Concurrent.Chan (newChan)
import           Data.IORef
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Text               (Text)
import qualified Data.Text.Lazy          as TL
import           Text.Markdown
import           Yesod
import           Yesod.Auth
import           Yesod.Auth.Dummy

-- This application will include the chat subsite and a wiki. The first thing 
-- we need to consider is how to store the wiki contents. Normally, we’d want 
-- to put this in some kind of a Persistent database. For simplicity, we’ll 
-- just use an in-memory representation. Each Wiki page is indicated by a list 
-- of names, and the contents of each page is going to be a piece of Text
data App = App
    { getChat     :: Chat
    , wikiContent :: IORef (Map [Text] Text)
    }

--set up routes
mkYesod "App" [parseRoutes|
/               HomeR GET           -- the home page
/wiki/*Texts    WikiR GET POST      -- note the multipiece for wiki hierarchy
/chat           ChatR Chat getChat  -- the chat Subsite
/auth           AuthR Auth getAuth  -- the auth subsite
|]

instance Yesod App where
    authRoute _ = Just $ AuthR LoginR -- get a working login link
    
    -- Our custom defaultLayout will add the chat widget to every page.
    -- We'll also add login and logout links to the top
    defaultLayout widget = do
        pc <- widgetToPageContent $ do
            widget
            chatWidget ChatR
        mmsg <- getMessage
        withUrlRenderer
            [hamlet|
                $doctype 5
                <html>
                    <head>
                        <title>#{pageTitle pc}
                        ^{pageHead pc}
                    <body>
                        $maybe msg <- mmsg
                            <div .message>#{msg}
                        <nav>
                            <a href=@{AuthR LoginR}>Login
                            \ | #
                            <a href=@{AuthR LogoutR}>Logout
                        ^{pageBody pc}
            |]

-- Since we’re using the chat subsite, we have to provide an instance of YesodChat.
instance YesodChat App where
    getUserName = do
        muid <- maybeAuthId
        case muid of
            Nothing -> do
                setMessage "Not logged in"
                redirect $ AuthR LoginR
            Just uid -> return uid
    
    isLoggedIn = do
        muid <- maybeAuthId
        return $ maybe False (const True) muid

-- Fairly standard YesodAuth instance. We'll just use the dummy plugin so that
-- you can create any name you want and store the login name as the AuthId
instance YesodAuth App where
    type AuthId App = Text
    authPlugins _ = [authDummy]
    loginDest _ = HomeR
    logoutDest _ = HomeR
    getAuthId = return . Just . credsIdent
    authHttpManager = error "authHttpManager" -- not used by dummy
    maybeAuthId = lookupSession "_ID"

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- nothinf special here, just giving a link to the root of the wiki
getHomeR :: Handler Html
getHomeR = defaultLayout
    [whamlet|
        <p>Welcome to the WIKI
        <p>
            <a href=@{wikiRoot}>Wiki root
    |]
  where
    wikiRoot = WikiR []

-- a formfor getting wiki content
wikiForm :: Maybe Textarea -> Html -> MForm Handler (FormResult Textarea, Widget)
wikiForm mtext = renderDivs $ areq textareaField "Page Body" mtext

-- show a wiki page and an edit form
getWikiR :: [Text] -> Handler Html
getWikiR page = do
    -- Get the reference to the contents map
    icontent <- fmap wikiContent getYesod

    -- And read the map from inside the reference
    content <- liftIO $ readIORef icontent

    -- Lookup the content of the current page if avaliable
    let mtext = Map.lookup page content

    -- Generate a form with the current contetns as a default value.
    -- Note that we use Textarea wrapper to get a <textarea>
    (form, _) <- generateFormPost $ wikiForm $ fmap Textarea mtext
    defaultLayout $ do
        case mtext of
            -- We're treating the input as markdown. The markdown package
            -- automatically handles XSS protection for us.
            Just text -> toWidget $ markdown def $ TL.fromStrict text
            Nothing -> [whamlet|<p>Page does not exist|]
        [whamlet|
            <h2>Edit page
            <form method=post>
                ^{form}
                <div>
                    <input type=submit>
        |]

-- Get a submitted wiki page and update the contents.
postWikiR :: [Text] -> Handler Html
postWikiR page = do
    icontent <- fmap wikiContent getYesod
    content <- liftIO $ readIORef icontent
    let mtext = Map.lookup page content

    ((res, form), _) <- runFormPost $ wikiForm $ fmap Textarea mtext
    case res of
        FormSuccess (Textarea t) -> do
            liftIO $ atomicModifyIORef icontent $
                \m -> (Map.insert page t m, ())
            setMessage "Page Updated"
            redirect $ WikiR page
        _ -> do
            setMessage "Page Update Error"
            defaultLayout
                [whamlet|
                    <form method=post>
                        ^{form}
                        <div>
                            <input type=submit>
                |]

main :: IO ()
main = do
    -- Create our server event channel
    chan <- newChan

    -- Initially have a blank inmemory database
    icontent <- newIORef Map.empty

    -- run app
    warpEnv App
        { getChat = Chat chan
        , wikiContent = icontent
        }


