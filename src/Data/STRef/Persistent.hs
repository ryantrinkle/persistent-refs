{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.STRef.Persistent
-- Copyright   :  (c) Adam C. Foltzer 2013
-- License     :  BSD3
--
-- Maintainer  :  acfoltzer@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (requires rank-2 types for runST)
--
-- Mutable references in the persistent
-- 'Control.Monad.ST.Persistent.ST' monad.
--
-----------------------------------------------------------------------------

module Data.STRef.Persistent (
    -- * 'STRef's
    STRef
  , asInt
    -- * 'MonadRef' Operations
  , MonadRef(..)
  , newSTRef
  , readSTRef
  , writeSTRef
  ) where

import Control.Monad.State
import Control.Monad.Ref
import Control.Monad.ST.Persistent.Internal
import Data.IntMap as IntMap
import Unsafe.Coerce

newtype STRef s a = STRef Int
    deriving (Eq, Show)

-- | Get the underlying 'Int' from an 'STRef'. Useful for debugging.
asInt :: STRef s a -> Int
asInt (STRef i) = i

newSTRef :: Monad m => a -> STT s m (STRef s a)
newSTRef x = STT $ do
    s <- get
    let i = next s
        heap' = insert i (unsafeCoerce x) (heap s)
        next' = next s + 1
    put $ s { heap = heap', next = next' }
    return (STRef i)

readSTRef :: (Monad m, s :< s') => STRef s a -> STT s' m a
readSTRef (STRef i) = STT $ do
  Just x <- liftM (IntMap.lookup i) $ gets heap
  return $ unsafeCoerce x

writeSTRef :: (Monad m, s :< s') => STRef s a -> a -> STT s' m ()
writeSTRef (STRef i) x = STT $ do
    s <- get
    let heap' = insert i (unsafeCoerce x) (heap s)
    put $ s { heap = heap' }

instance Monad m => MonadRef (STRef s) (STT s m) where
    newRef   = newSTRef
    readRef  = readSTRef
    writeRef = writeSTRef
