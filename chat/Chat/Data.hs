{--
    This example will tie together a few different ideas. We'll start with a chat subsite,
    which allows us to embed a chat widget on any pagge. We'll use the HTML 5 event
    source API to handle sendig events from the server to the client.
--}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Chat.Data where

{--
 - In order to define a subsite, we first need to create a subsite foundation 
 - type for the subsite, the same as we would do for a normal Yesod application. 
 - In our case, we want to keep a channel of all the events to be sent to the 
 - individual participants of a chat. This ends up looking like:
--}
import           Blaze.ByteString.Builder.Char.Utf8  (fromText)
import           Control.Concurrent.Chan
import           Data.Monoid                         ((<>))
import           Data.Text                           (Text)
import           Network.Wai.EventSource
import           Network.Wai.EventSource.EventStream
import           Yesod
-- Our subsite foundatioon.
-- We keep a channel of events that all connections will share

data Chat = Chat (Chan ServerEvent)

{--
 - We also need to define our subsite routes in the same module. We need to have 
 - two commands: one to send a new message to all users, and another to receive 
 - the stream of messages.
--}
mkYesodSubData "Chat" [parseRoutes|
/send SendR POST
/recv ReceiveR GET
|]

{--
 - Now that we’ve defined our foundation and routes, we need to create a separate 
 - module for providing the subsite dispatch functionality. We’ll call this module 
 - Chat, and it’s where we’ll start to see how a subsite functions.

 - A subsite always sits as a layer on top of some master site, which will be 
 - provided by the user. In many cases, a subsite will require specific 
 - functionality to be present in the master site. In the case of our chat subsite, 
 - we want user authentication to be provided by the master site. The subsite needs 
 - to be able to query whether the current user is logged into the site, and to 
 - get the user’s name.

 - The way we represent this concept is to define a typeclass that encapsulates 
 - the necessary functionality. Let’s have a look at our YesodChat typeclass:
--}
class (Yesod master, RenderMessage master FormMessage) => YesodChat master where
    getUserName :: HandlerT master IO Text
    isLoggedIn :: HandlerT master IO Bool

{--
 - Any master site which wants to use the chat subsite will need to provide a 
 - YesodChat instance. (We’ll see in a bit how this requirement is enforced.) 
 - There are a few interesting things to note:

    * We can put further constraints on the master site, such as providing a Yesod 
      instance and allowing rendering of form messages. The former allows us to 
      use defaultLayout, while the latter allows us to use standard form widgets.

    * Previously in the book, we’ve used the Handler monad quite a bit. Remember 
      that Handler is just an application-specific type synonym around HandlerT. 
      Since this code is intended to work with many different applications, we use 
      the full HandlerT form of the transformer.

 - Speaking of the Handler type synonym, we’re going to want to have something 
 - similar for our subsite. The question is: what does this monad look like? 
 - In a subsite situation, we end up with two layers of HandlerT transformers: 
 - one for the subsite, and one for the master site. We want to have a synonym 
 - that works for any master site which is an instance of YesodChat, so we end 
 - up with:
--}
type ChatHandler a =
    forall master. YesodChat master => HandlerT Chat (HandlerT master IO) a

{--
 - Now that we have our machinery out of the way, it’s time to write our subsite 
 - handler functions. We had two routes: one for sending messages, and one for 
 - receiving messages. Let’s start with sending. We need to:

    1. Get the username for the person sending the message.

    2. Parse the message from the incoming parameters. (Note that we’re going 
       to use GET parameters for simplicity of the client-side Ajax code.)

    3. Write the message to the Chan.

The trickiest bit of all this code is to know when to use lift.
--}
postSendR :: ChatHandler ()
postSendR = do
    -- getUserName is the function we defined in our YesodChat typeclass earlier. 
    -- If we look at that type signature, we see that it lives in the master site’s 
    -- Handler monad. Therefore, we need to lift that call out of the subsite.
    from <- lift getUserName

    -- The call to runInputGet is a little more subtle. Theoretically, we could 
    -- run this in either the subsite or the master site. However, we use lift 
    -- here as well for one specific reason: message translations. By using the 
    -- master site, we can take advantage of whatever RenderMessage instance the 
    -- master site defines. This also explains why we have a RenderMessage 
    -- constraint on the YesodChat typeclass.
    body <- lift $ runInputGet $ ireq textField "message"
    
    -- The next call to getYesod is not lifted. The reasoning here is simple: we 
    -- want to get the subsite’s foundation type in order to access the message 
    -- channel. If we instead lifted that call, we’d get the master site’s foundation 
    -- type instead, which is not what we want in this case.
    Chat chan <- getYesod

    -- The final line puts the new message into the channel. Since this is an IO 
    -- action, we use liftIO. ServerEvent is part of the wai-eventsource package, 
    -- and is the means by which we’re providing server-sent events in this example.
    liftIO $ writeChan chan $ ServerEvent Nothing Nothing $ return $
        fromText from <> fromText ": " <> fromText body

-- The receiving side is similarly simple:
-- We use dupChan so that each new connection receives its own copy of newly 
-- generated messages. This is a standard method in concurrent Haskell of 
-- creating broadcast channels. The last line in our function exposes the 
-- underlying wai-eventsource application as a Yesod handler, using the 
-- sendWaiApplication function to promote a WAI application to a Yesod handler.
getReceiveR :: ChatHandler ()
getReceiveR = do
    Chat chan0 <- getYesod
    chan <- liftIO $ dupChan chan0
    sendWaiApplication $ eventSourceAppChan chan



