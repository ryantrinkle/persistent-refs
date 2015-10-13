{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.ST.Persistent.Internal where

import Control.Monad.State.Strict
import Data.Functor.Identity
import Data.IntMap (IntMap, empty)
import GHC.Base (Any)

#if MIN_VERSION_base(4,8,0)
import Control.Applicative (Alternative)
#else
import Control.Applicative (Alternative, Applicative)
#endif

data Heap = Heap { heap :: IntMap Any, next :: Int }

emptyHeap :: Heap
emptyHeap = Heap { heap = empty, next = minBound }

-- | A persistent version of the 'Control.Monad.ST.ST' monad.
type ST s = STT s Identity

-- | Run a computation that uses persistent references, and return a
-- pure value. The rank-2 type offers similar guarantees to
-- 'Control.Monad.ST.runST'.
runST :: (forall s. ST s a) -> a
runST m = runIdentity (runSTT m)

newtype STT s m a = STT { unSTT :: StateT Heap m a }
    deriving (Functor, Applicative, Alternative, Monad, MonadIO, MonadPlus, MonadTrans)

-- | Run a computation that uses persistent references, and return a
-- pure value. The rank-2 type offers similar guarantees to
-- 'Control.Monad.ST.runST'.
runSTT :: Monad m => (forall s. STT s m a) -> m a
runSTT (STT c) = evalStateT c emptyHeap

data STTCont a = forall s. STTCont (a s, Heap)

runSTT' :: Monad m => (forall s. STT s m (a s)) -> m (STTCont a)
runSTT' (STT c) = liftM STTCont $ runStateT c emptyHeap

data Succ :: * -> *

class a :< b
instance a :< a
instance {-# OVERLAPPABLE #-} (a :< b, b :< c) => a :< c
instance {-# OVERLAPS #-} a :< Succ a

continueSTT' :: forall a b m. Monad m => (forall s. a s -> STT s m (b s)) -> STTCont a -> m (STTCont b)
continueSTT' c (STTCont (a, h)) = liftM STTCont $ runStateT (unSTT $ c a) h

continueSTT :: forall a b m. Monad m => (forall s. a s -> STT s m b) -> STTCont a -> m b
continueSTT c (STTCont (a, h)) = evalStateT (unSTT $ c a) h

newtype STTSuspension r a = STTSuspension { unSTTSuspension :: (a r, Heap) }

suspendSTT :: Monad m => a s -> (forall r. STTSuspension r a -> m (STTSuspension r b)) -> STT s m (b s)
suspendSTT a f = STT $ do
  h <- get
  STTSuspension (b, h') <- lift $ f $ STTSuspension (a, h)
  put h'
  return b

resumeSTT :: Monad m => STTSuspension r a -> (forall s. a s -> STT s m (b s)) -> m (STTSuspension r b)
resumeSTT (STTSuspension (a, h)) f = liftM STTSuspension $ runStateT (unSTT $ f a) h

suspensionToCont :: STTSuspension r a -> STTCont a
suspensionToCont (STTSuspension x) = STTCont x
