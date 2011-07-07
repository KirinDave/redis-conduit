module Tap.Redis (AtomicMessageStore, intoHistory) where 
       
import qualified Database.Redis.Redis as Redis

import Data.Enumerator (Iteratee(..), Stream(..), Step(..), returnI, liftI, continue, yield)
import qualified Data.Enumerator.Redis as R

import qualified Data.Sequence as Seq
import Data.Sequence ((><), Seq(..))

import Control.Monad (forM_)
import Control.Concurrent.MonadIO (liftIO, MonadIO)
import Data.ByteString (ByteString)
import Control.Concurrent.STM.TVar (TVar, newTVar,readTVar,writeTVar)
import Control.Monad.STM (atomically)

type RedisMessage = Redis.Message ByteString
type MessageStore = Seq RedisMessage
type AtomicMessageStore = TVar MessageStore

max_buffer_size = 64

enqMessage :: AtomicMessageStore -> [RedisMessage] -> IO ()
enqMessage tv xs = do
  atomically $ do 
    history <- readTVar tv
    let concated = Seq.fromList xs >< history in
      writeTVar tv (Seq.take max_buffer_size concated)
enqMessage _ _ = return ()          
    

intoHistory :: TVar MessageStore -> Iteratee RedisMessage IO ()
intoHistory tvar = continue step where
	step (Chunks []) = continue step
	step (Chunks xs) = liftIO (enqMessage tvar xs) >> continue step
	step EOF = yield () EOF


  
  
  

