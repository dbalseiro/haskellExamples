{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Data.IntMap            (IntMap)
import qualified Data.IntMap            as IntMap
import           Data.Text              (Text)
import           Yesod

-- We’ll need two different mutable references in our foundation. The first will 
-- keep track of the next "job id" we’ll hand out. Each of these background jobs 
-- will be represented by a unique identifier, which will be used in our URLs. 
-- The second piece of data will be a map from the job ID to the broadcast 
-- channel used for publishing updates
data App = App
    { jobs      :: TVar (IntMap (TChan (Maybe Text)))
    , nextJob   :: TVar Int
    }

mkYesod "App" [parseRoutes|
/ HomeR GET POST
/view-progress/#Int ViewProgressR GET
|]

instance Yesod App

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "PubSub"
    [whamlet|
        <form method=post>
            <input type=submit value="new bg job">
    |]

postHomeR :: Handler ()
postHomeR = do
    App {..} <- getYesod

    -- In order to allocate a job, we need
    --   1. Get a job id
    --   2. Create a new broadcast channel
    --   3. Add the channel to the channel map
    (jobId, chan) <- liftIO $ atomically $ do
        jobId <- readTVar nextJob
        writeTVar nextJob $! jobId + 1
        chan <- newBroadcastTChan
        m <- readTVar jobs
        writeTVar jobs $ IntMap.insert jobId chan m
        return (jobId, chan)

    -- Fork BG job. There are many different ways we could go about this, and 
    -- they depend entirely on what the background job is going to be. Here’s a 
    -- minimal example of a background job that prints out a few messages, with 
    -- a 1 second delay between each message. Note how after our final message, 
    -- we broadcast a Nothing value and remove our channel from the map of 
    -- channels.
    liftIO $ forkIO $ do
        threadDelay 1000000
        atomically $ writeTChan chan $ Just "Did something\n"
        threadDelay 1000000
        atomically $ writeTChan chan $ Just "Did something else\n"
        threadDelay 51000000
        atomically $ writeTChan chan $ Just "Did another thing\n"
        threadDelay 1000000
        atomically $ do
            writeTChan chan $ Just "Done\n"
            writeTChan chan Nothing
            m <- readTVar jobs
            writeTVar jobs $ IntMap.delete jobId m
    redirect $ ViewProgressR jobId

getViewProgressR :: Int -> Handler TypedContent
getViewProgressR jobId = do
    App {..} <- getYesod
    -- We start off by looking up the channel in the map. If we can’t find it, 
    -- it means the job either never existed, or has already been completed. In 
    -- either event, we return a 404. (Another possible enhancement would be to 
    -- store some information on all previously completed jobs and let the user 
    -- know if they’re done.)

    -- Assuming the channel exists, we use respondSource to start a streaming 
    -- response. We then repeatedly call readTChan until we get a Nothing value, 
    -- at which point we exit (via return ()). Notice that on each iteration, we 
    -- call both sendChunkText and sendFlush. Without that second call, the user 
    -- won’t receive any updates until the output buffer completely fills up, 
    -- which is not what we want for a real-time update system.
    mchan <- liftIO $ atomically $ do
        m <- readTVar jobs
        case IntMap.lookup jobId m of
            Nothing -> return Nothing
            Just chan -> fmap Just $ dupTChan chan
    case mchan of
        Nothing -> notFound
        Just chan -> respondSource typePlain $ do
            let loop = do
                mtext <- liftIO $ atomically $ readTChan chan
                case mtext of
                    Nothing -> return ()
                    Just text -> do
                        sendChunkText text
                        sendFlush
                        loop
            loop

main :: IO ()
main = do
    jobs <- newTVarIO IntMap.empty
    nextJob <- newTVarIO 1
    warp 3000 App {..}

