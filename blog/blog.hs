{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Yesod
import Yesod.Auth
import Yesod.Auth.Dummy
import Yesod.Form.Nic
    ( YesodNic
    , nicHtmlField
    )

import Database.Persist.Sqlite
    ( ConnectionPool
    , SqlBackend
    , runSqlPool
    , runMigration
    , createSqlitePool
    , runSqlPersistMPool
    )

import Data.Text (Text)
import Data.Time
    ( UTCTime
    , getCurrentTime
    )
import Data.Typeable (Typeable)

import Network.HTTP.Conduit
    ( Manager
    , newManager
    )
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Control.Monad.Logger (runStdoutLoggingT)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    email       Text
    UniqueUser  email
    deriving    Typeable

Entry
    title   Text
    posted  UTCTime
    content Html

Comment
    entry   EntryId
    posted  UTCTime
    user    UserId
    name    Text
    text    Textarea
|]

data Blog = Blog
    { connPool :: ConnectionPool
    , httpManager :: Manager
    }

mkMessage "Blog" "blog-messages" "en"

mkYesod "Blog" [parseRoutes|
/              HomeR  GET
/Blog          BlogR  GET POST
/blog/#EntryId EntryR GET POST
/auth          AuthR  Auth getAuth
|]

instance Yesod Blog where
    approot = ApprootStatic "http://localhost:3000"

    isAuthorized BlogR True = do
        mauth <- maybeAuth
        case mauth of
          Nothing -> return AuthenticationRequired
          Just (Entity _ user)
            | isAdmin user -> return Authorized
            | otherwise -> return AuthenticationRequired

    isAuthorized (EntryR _) True = do
        mauth <- maybeAuth
        case mauth of
          Nothing -> return AuthenticationRequired
          Just _ -> return Authorized

    isAuthorized _ _ = return Authorized

    authRoute _ = Just (AuthR LoginR)

    defaultLayout inside = do
        mmsg <- getMessage
        pc <- widgetToPageContent $ do
            toWidget [lucius|
body {
    width: 760px;
    margin: 1em auto;
    font-family: sans-serif;
}
textarea {
    width: 400px;
    height: 200px;
}
 #message {
  color: #900;
}
|]
            inside
        withUrlRenderer [hamlet|
$doctype 5
<html>
    <head>
        <title>#{pageTitle pc}
        ^{pageHead pc}
    <body>
        $maybe msg <- mmsg
            <div #message>#{msg}
        ^{pageBody pc}
|]

instance YesodPersist Blog where
    type YesodPersistBackend Blog = SqlBackend
    runDB f = do
        master <- getYesod
        let pool = connPool master
        runSqlPool f pool

type Form x = Html -> MForm Handler (FormResult x, Widget)

instance RenderMessage Blog FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodNic Blog

instance YesodAuth Blog where
    type AuthId Blog = UserId
    loginDest _ = HomeR
    logoutDest _ = HomeR
    authHttpManager = httpManager

    authPlugins _ = [authDummy]

    getAuthId creds = do
        let email = credsIdent creds
            user = User email
        res <- runDB $ insertBy user
        return $ Just $ either entityKey id res

instance YesodAuthPersist Blog

isAdmin :: User -> Bool
isAdmin = (== "dbalseiro@gmail.com") . userEmail

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitleI MsgHomepageTitle
    [whamlet|
<p>_{MsgWelcomeHomepage}
<p>
    <a href=@{BlogR}>_{MsgSeeArchive}
|]

entryForm :: Form Entry
entryForm = renderDivs $ Entry
    <$> areq textField (fieldSettingsLabel MsgNewEntryTitle) Nothing
    <*> lift (liftIO getCurrentTime)
    <*> areq nicHtmlField (fieldSettingsLabel MsgNewEntryContent) Nothing

getBlogR :: Handler Html
getBlogR = do
    mauth <- maybeAuth
    entries <- runDB $ selectList [] [Desc EntryPosted]
    (entryWidget, encType) <- generateFormPost entryForm
    defaultLayout $ do
        setTitleI MsgBlogArchiveTitle
        [whamlet|
$if null entries
    <p>_{MsgNoEntries}
$else
    <ul>
        $forall Entity entryId entry <- entries
            <a href=@{EntryR entryId}>#{entryTitle entry}
$maybe Entity _ user <- mauth
    $if isAdmin user
        <form method=post enctype=#{encType}>
            ^{entryWidget}
            <input type=submit value=_{MsgNewEntry}>
$nothing
    <p>
        <a href=@{AuthR LoginR}>_{MsgLoginToPost}
|]

postEntryR :: EntryId -> Handler Html
postEntryR entryId = do
    ((res, commentWidget), encType) <- runFormPost $ commentForm entryId
    case res of
      FormSuccess comment -> do
          _ <- runDB $ insert comment
          setMessageI MsgCommentAdded
          redirect $ EntryR entryId
      _ -> defaultLayout $ do
          setTitleI MsgPleaseCorrectComment
          [whamlet|
<form method=post enctype=#{encType}>
    ^{commentWidget}
    <input type=submit value=_{MsgAddCommentButton}>
|]
    

postBlogR :: Handler Html
postBlogR = do
    ((res, entryWidget), encType) <- runFormPost entryForm
    case res of
      FormSuccess entry -> do
          entryId <- runDB $ insert entry
          setMessageI $ MsgEntryCreated (entryTitle entry)
          redirect $ EntryR entryId
      _ -> defaultLayout $ do
          setTitleI MsgPleaseCorrectEntry
          [whamlet|
<form method=post enctype=#{encType}>
    ^{entryWidget}
    <input type=submit value=_{MsgNewEntry}>
|]

commentForm :: EntryId -> Form Comment
commentForm entryId = renderDivs $ Comment
    <$> pure entryId
    <*> lift (liftIO getCurrentTime)
    <*> lift requireAuthId
    <*> areq textField (fieldSettingsLabel MsgCommentName) Nothing
    <*> areq textareaField (fieldSettingsLabel MsgCommentText) Nothing

getEntryR :: EntryId -> Handler Html
getEntryR entryId = do
    (entry, comments) <- do
        entry <- runDB $ get404 entryId
        comments <- runDB $ selectList [CommentEntry ==. entryId] [Desc CommentPosted]
        return (entry, map entityVal comments)
    muser <- maybeAuth
    (commentWidget, encType) <- generateFormPost $ commentForm entryId
    defaultLayout $ do
        setTitleI $ MsgEntryTitle (entryTitle entry)
        [whamlet|
<h1>#{entryTitle entry}
<article>#{entryContent entry}
    <section .comments>
        <h1>_{MsgCommentsHeading}
        $if null comments
            <p>_{MsgNoComments}
        $else
            $forall Comment _entry posted _user name text <- comments
                <div .comment>
                <span .by>#{name}
                <span .at>#{show posted}
                <div .content>#{text}

        <section>
            <h1>_{MsgAddCommentHeading}
            $maybe _ <- muser
                <form method=post enctype=#{encType}>
                    ^{commentWidget}
                    <input type=submit value=_{MsgAddCommentButton}>
            $nothing
                <p>
                    <a href=@{AuthR LoginR}>_{MsgLoginToComment}
|]


main :: IO ()
main = do
    --create a new pool
    pool <- runStdoutLoggingT $ createSqlitePool "blog.db3" 10
    --perform any necesary migration
    runSqlPersistMPool (runMigration migrateAll) pool
    --create a new HTTP Manager
    manager <- newManager tlsManagerSettings
    --start server
    warp 3000 $ Blog pool manager
