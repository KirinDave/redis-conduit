module Tap.Redis (AtomicMessageStore
                 , MessageStore
                 , RedisMessage
                 , storeChannelsInto
                 , clearChannel
                 , checkChannel
                 , ) where 

import Prelude hiding (take)
import Database.Redis.Redis (Message(..), Redis, subscribe)
import Data.Enumerator (Iteratee(..), Stream(..),
                        continue, yield, ($$), 
                        run_)
import Data.Enumerator.Redis (enumSubscriptions)

import Data.Sequence ((><), Seq, fromList, take, empty)
import Data.ByteString (ByteString)

import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.MonadIO (liftIO, MonadIO)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar


type RedisMessage = Message ByteString
type MessageStore = Seq RedisMessage
type AtomicMessageStore = TVar MessageStore

max_buffer_size :: Int
max_buffer_size = 1024

intoHistory :: AtomicMessageStore -> Iteratee RedisMessage IO ()
intoHistory tvar = continue step where
	step (Chunks []) = continue step
	step (Chunks xs) = (liftIO (enqMessages tvar xs)) >> continue step
	step EOF = yield () EOF

enqMessages :: AtomicMessageStore -> [RedisMessage] -> IO ()
enqMessages tv xs =  
  atomically $ do 
    history <- readTVar tv
    let concated = fromList xs >< history in
      writeTVar tv (take max_buffer_size concated)

storeChannelsInto :: Redis -> [String] -> IO (AtomicMessageStore, ThreadId)
storeChannelsInto redis channels = do
  _ <- subscribe redis channels :: IO [Message ()] -- We ignore the result, we don't care, so don't.
  newStore <- newTVarIO empty
  spawned  <- forkIO $ do
              run_ ( enumSubscriptions 1000 redis $$ intoHistory newStore )
  return (newStore, spawned)

clearChannel :: AtomicMessageStore -> IO MessageStore
clearChannel ams = 
  atomically $ do
    v <- readTVar ams
    writeTVar ams empty 
    return v
    
checkChannel :: AtomicMessageStore -> IO MessageStore
checkChannel ams = 
  atomically $ readTVar ams >>= return
    