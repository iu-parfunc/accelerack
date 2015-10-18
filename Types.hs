{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Control.Monad

data Type
  = Tuple  Tup
  | Scalar Scalar
  deriving (Eq,Ord,Show)

instance Storable Type where
  sizeOf _ = szInt
           + max szInt
                 (sizeOf (undefined :: Ptr ()))
  alignment _ = 8
  peek p = do
    tag :: CInt <- peek $ castPtr p
    case tag of
      -- SCALAR
      0 -> Scalar <$> peekByteOff (castPtr p) szInt
      -- TUPLE
      1 -> Tuple  <$> peekByteOff (castPtr p) szInt
      _ -> fail $ "Unrecognized Type tag: " ++ show tag
  poke p = \case
    Scalar s -> do
      poke (castPtr p :: Ptr CInt) 0
      pokeByteOff p szInt p
    Tuple  t -> do
      poke (castPtr p :: Ptr CInt) 1
      pokeByteOff p szInt p

newtype Tup = Tup
  { getTuple :: [Type]
  } deriving (Eq,Ord,Show)

instance Storable Tup where
  sizeOf    _ = szInt + szPtr
  alignment _ = 8
  peek p = do
    len :: CInt <- peek $ castPtr p
    let offs = take (fromCInt len) $ iterate (szPtr +) szInt
    fmap Tup $ forM offs $ peekByteOff (castPtr p)
  poke p (Tup ts) = do
    let len = toCInt $ length ts
    poke (castPtr p) len
    let ots = zip (iterate (szPtr +) szInt) ts
    forM_ ots $ \(off,t) -> pokeByteOff p off t

data Scalar
  = Double
  | Bool
  | Int
  deriving (Eq,Ord,Show,Bounded,Enum)

instance Storable Scalar where
  sizeOf    _ = szInt
  alignment _ = alignment (undefined :: CInt)
  peek p = do
    i :: CInt <- peek $ castPtr p
    when (i < minBound || i > maxBound)
      $ fail $ "Unrecognized Scalar type: " ++ show i
    return $ fromCInt i
  poke p = poke (castPtr p) . toCInt

toCInt :: Enum a => a -> CInt
toCInt = toEnum . fromEnum

fromCInt :: Enum a => CInt -> a
fromCInt = toEnum . fromEnum

szInt :: Int
szInt = sizeOf (undefined :: CInt)

szPtr :: Int
szPtr = sizeOf (undefined :: Ptr ())

