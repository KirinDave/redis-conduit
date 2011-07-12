{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Controller
    ( withTap
    , withDevelApp
    ) where

import Tap
import Tap.Redis
import qualified Database.Redis.Redis as Redis
import Settings
import Yesod.Helpers.Static
import Data.ByteString (ByteString)
import Network.Wai (Application)
import Data.Dynamic (Dynamic, toDyn)

-- Import all relevant handler modules here.
import Handler.Root
import Handler.Display

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Tap.hs. Please see
-- the comments there for more details.
mkYesodDispatch "Tap" resourcesTap

-- Some default handlers that ship with the Yesod site template. You will
-- very rarely need to modify this.
getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "config/favicon.ico"

getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent ("User-agent: *" :: ByteString)

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
withTap :: (Application -> IO a) -> IO a
withTap f = do
    redisC <- localRedis
    (ams, _) <- storeChannelsInto redisC ["metrics:statman"]
    let h = Tap s ams
    toWaiApp h >>= f
  where
    s = static Settings.staticdir

localRedis :: IO Redis.Redis
localRedis = Redis.connect "127.0.0.1" Redis.defaultPort

withDevelApp :: Dynamic
withDevelApp = toDyn (withTap :: (Application -> IO ()) -> IO ())
