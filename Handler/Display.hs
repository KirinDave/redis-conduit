{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Display where

import Yesod.Json (jsonList, Json(..))
import Yesod.Content

import Tap
import Tap.Redis
import Control.Concurrent.MonadIO (liftIO, MonadIO)
import Database.Redis.Redis (Message(..))
import qualified Data.ByteString as B
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Data.List     as L

--  do Tap _ messages <- getYesod
--     queue <- liftIO $ clearChannel messages

--getDisplayTap :: Handler (ContentType, Content)
getDisplayTap :: Handler RepJson
getDisplayTap = do
  Tap _ mstore <- getYesod
  queue <- liftIO $ clearChannel mstore
  --return (typeJson, toContent $ jsonArrayify queue)
  return $ RepJson $ (toContent . jsonArrayify) queue


jsonArrayify :: MessageStore -> B.ByteString
jsonArrayify queue = 
  let msgList = (buildList $ getMessage `fmap` queue) 
      csml    = L.intersperse "," msgList in
  B.concat $ ["["] ++ csml ++ ["]"]
  where getMessage (MMessage _ s) = s
        getMessage _              = ""
        buildList = F.foldr (:) []

