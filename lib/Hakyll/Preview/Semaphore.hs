module Hakyll.Preview.Semaphore
  ( Sema
  , newSema
  , readSema
  , putSema
  , tryPutSema
  , takeSema
  , tryTakeSema
  ) where

import Control.Concurrent
import Control.Monad (void)

type Sema = MVar Bool

newSema :: IO Sema
newSema = newEmptyMVar 

readSema :: Sema -> IO Bool
readSema = readMVar

putSema :: Sema -> Bool -> IO ()
putSema = putMVar

tryPutSema :: Sema -> Bool -> IO Bool
tryPutSema = tryPutMVar

takeSema :: Sema -> IO Bool
takeSema = takeMVar

tryTakeSema :: Sema -> IO (Maybe Bool)
tryTakeSema = tryTakeMVar

