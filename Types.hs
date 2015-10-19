{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Types where

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable

-- Arr {{{

data Arr = Arr
  { arrShape :: [Int]
  , arrData  :: [Vec]
  }

peekArr :: Ptr a -> IO Arr
peekArr p = do
  psh <- peekByteOff p 0
  pvs <- peekArray0 nullPtr (p `plusPtr` ptrSize)
  sh  <- peekShape psh
  let len = product sh
  Arr sh <$> mapM (peekVec len) pvs

peekShape :: Ptr CInt -> IO [Int]
peekShape = fmap (map fromCInt) . peekArray0 (-1)

-- }}}

-- Scalar {{{

data Scalar a where
  Double :: Scalar Double
  Int    :: Scalar Int
  Bool   :: Scalar Bool

deriving instance Eq   (Scalar a)
deriving instance Ord  (Scalar a)
deriving instance Show (Scalar a)

peekScalar :: Ptr a -> IO AScalar
peekScalar p = do
  i :: CInt <- peek $ castPtr p
  case i of
    0 -> return $ AScalar Double
    1 -> return $ AScalar Int
    2 -> return $ AScalar Bool
    _ -> fail $ "Unknown type tag: " ++ show i

-- }}}

-- Vec {{{

data Vec where
  (:::) :: Scalar a -> [a] -> Vec
infixr 5 :::

data AScalar where
  AScalar :: Storable a => Scalar a -> AScalar

withScalar :: AScalar -> (forall a. Storable a => Scalar a -> r) -> r
withScalar (AScalar t) f = f t

peekVec :: Int -> Ptr a -> IO Vec
peekVec len p = do
  t  <- peekScalar p
  withScalar t $ \typ -> do
    p' <- peekByteOff p ptrSize
    (:::) typ <$> peekArray len p'

-- }}}

fromCInt :: Enum a => CInt -> a
fromCInt = toEnum . fromEnum

toCInt :: Enum a => a -> CInt
toCInt = toEnum . fromEnum

ptrSize :: Int
ptrSize = sizeOf nullPtr

