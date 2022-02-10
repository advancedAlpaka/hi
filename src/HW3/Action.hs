{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
module HW3.Action
  where
import Control.Exception (Exception)
import Data.Set (Set)

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime deriving (Show, Eq, Ord, Bounded, Enum)

newtype PermissionException =
  PermissionRequired HiPermission deriving Show

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a } deriving(Functor)

instance Functor HIO => Applicative HIO where
  pure a = HIO $ \_ -> return a
  (HIO as) <*> (HIO bs) = HIO $ \perms -> do
    a <- as perms
    b <- bs perms
    pure (a b)

instance Applicative HIO => Monad HIO where
  (HIO as) >>= f = HIO $ \perms -> do
    a <- as perms
    runHIO (f a) perms
