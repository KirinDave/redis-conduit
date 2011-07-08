module Tap.Redis (AtomicMessageStore
                 , MessageStore
                 , RedisMessage
                 , storeChannelsInto) where 

import Prelude hiding (take)
import Database.Redis.Redis (Message(..), Redis(..), subscribe)
import Data.Enumerator (Iteratee(..), Stream(..), Step(..), 
                        returnI, liftI, continue, yield, ($$), 
                        run_)
import Data.Enumerator.Redis (enumSubscriptions)

import Data.Sequence ((><), Seq(..), fromList, take, empty)
import Data.ByteString (ByteString)

import Control.Monad (forM_)
import Control.Concurrent (forkIO, ThreadId(..))
import Control.Concurrent.MonadIO (liftIO, MonadIO)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar


type RedisMessage = Message ByteString
type MessageStore = Seq RedisMessage
type AtomicMessageStore = TVar MessageStore

max_buffer_size = 64

enqMessages :: AtomicMessageStore -> [RedisMessage] -> IO ()
enqMessages tv xs =  
  atomically $ do 
    history <- readTVar tv
    let concated = fromList xs >< history in
      writeTVar tv (take max_buffer_size concated)

intoHistory :: AtomicMessageStore -> Iteratee RedisMessage IO ()
intoHistory tvar = continue step where
	step (Chunks []) = continue step
	step (Chunks xs) = (liftIO (enqMessages tvar xs)) >> continue step
	step EOF = yield () EOF


storeChannelsInto :: Redis -> [String] -> IO (AtomicMessageStore, ThreadId)
storeChannelsInto redis channels = do
  subscribe redis channels :: IO [Message ()] -- We ignore the result, we don't care, so don't
                                              -- allocate any space.
  newStore <- newTVarIO empty
  spawned  <- forkIO $ run_ ( enumSubscriptions redis 1000 $$ intoHistory newStore )
  return (newStore, spawned)

