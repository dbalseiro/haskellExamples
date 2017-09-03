{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Chat where

import           Chat.Data
import           Yesod

-- In a normal application, dispatching is handled by calling mkYesod, which 
-- creates the appropriate YesodDispatch instance. In subsites, things are a 
-- little bit more complicated, since you’ll often want to place constraints on 
-- the master site
--
-- We’re stating that our Chat subsite can live on top of any master site which 
-- is an instance of YesodChat. We then use the mkYesodSubDispatch Template 
-- Haskell function to generate all of our dispatching logic. While this is a 
-- bit more difficult to write than mkYesod, it provides necessary flexibility, 
-- and is mostly identical for any subsite you’ll write.
instance YesodChat master => YesodSubDispatch Chat (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesChat)

-- The final component we want as part of our chat library is a widget to be 
-- embedded inside a page which will provide chat functionality. By creating this 
-- as a widget, we can include all of our HTML, CSS, and Javascript as a reusable 
-- component.

-- Our widget will need to take in one argument: a function to convert a Chat 
-- subsite URL into a master site URL. The reasoning here is that an application 
-- developer could place the chat subsite anywhere in the URL structure, and this 
-- widget needs to be able to generate Javascript which will point at the 
-- correct URLs
chatWidget :: YesodChat master
           => (Route Chat -> Route master)
           -> WidgetT master IO ()
chatWidget toMaster = do
    chat <- newIdent    -- the containing div
    output <- newIdent  -- the box containing the messages
    input <- newIdent   -- input field from the user

    ili <- handlerToWidget isLoggedIn -- check if we're already logged in

    if ili
        then do
            -- Logged in: show the widget
            [whamlet|
                <div ##{chat}>
                    <h2>Chat
                    <div ##{output}>
                        <input ##{input} type=text placeholder="Enter Meassage">
            |]
            -- just some CSS
            toWidget [lucius|
                ##{chat} {
                    position: absolute;
                    top: 2em;
                    right: 2em;
                }
                ##{output} {
                    width: 200px;
                    height: 300px;
                    border: 1px solid #999;
                    overflow: auto;
                }
            |]
            -- and now some js
            toWidgetBody [julius|
                //set up receiving end
                var output = document.getElementById(#{toJSON output});
                var src = new EventSource("@{toMaster ReceiveR}");
                src.onmessage = function(msg) {
                    // this function will be called for each new message
                    var p = document.createElement("p");
                    p.appendChild(document.createTextNode(msg.data));
                    output.appendChild(p);

                    // And now scroll down whithin the output div so the most
                    // recent message is displayed
                    output.scrollTop = output.scrollHeight;
                };

                //Set up the sending end: send a message via Ajax whenever the
                //user hit enter
                var input = document.getElementById(#{toJSON input});
                input.onkeyup = function(event) {
                    var keycode = (event.keyCode ? event.keyCode : event.which);
                    if (keycode == '13') {
                        var xhr = new XMLHttpRequest();
                        var val = input.value;
                        input.value = "";
                        var params = "?message=" + encodeURI(val);
                        xhr.open("POST", "@{toMaster SendR}" + params);
                        xhr.send(null);
                    }
                };
            |]

            else do
                --User isn't loggedin, give him a message
                master <- getYesod
                [whamlet|
                    <p>
                        You must be #
                        $maybe ar <- authRoute master
                            <a href=@{ar}>logged in
                        $nothing
                            logged in
                        \ to chat
                |]
