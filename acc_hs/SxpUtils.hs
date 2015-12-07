{-# Language TypeOperators #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-} 
{-# LANGUAGE AllowAmbiguousTypes #-}

module ExUtils where

import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.Marshal.Array

import Control.Applicative
import Control.Monad
import Control.Monad.State

import Data.Array.Accelerate as A hiding ((++), replicate, product)
import Data.Array.Accelerate.IO (fromPtr, toPtr)
import Data.Array.Accelerate.Interpreter as I
import Data.Array.Accelerate.Array.Sugar as Sugar

import Data.IORef
import qualified Data.List as L

-- C structure to store tuple/scalar information (one payload)
data Segment = Segment { 
    slength :: CInt
  , stype :: CInt
  , sdata :: Ptr ()
} deriving Show

-- C structure to store accelerate arrays information (one logical array)
data AccArray = AccArray { 
    atype :: CInt
  , ashape :: Ptr Segment
  , adata :: Ptr Segment
} deriving Show

instance Storable Segment where
    sizeOf    _ = (16)
    alignment _ = alignment (undefined :: CString)

    poke p segment = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) p $ slength segment
        (\hsc_ptr -> pokeByteOff hsc_ptr 4) p $ stype segment
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) p $ sdata segment

    peek p = return Segment
              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 0) p)
              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 4) p)
              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 8) p)


instance Storable AccArray where
    sizeOf    _ = (24)
    alignment _ = alignment (undefined :: CString)

    poke p accarray = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) p $ atype accarray
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) p $ ashape accarray
        (\hsc_ptr -> pokeByteOff hsc_ptr 16) p $ adata accarray

    peek p = return AccArray
              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 0) p)
              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 8) p)
              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 16) p)

-- type Dim = DIM0 | DIM1 | DIM2 | DIM3
           -- deriving (Eq,Ord,Show)

-- Peek values from Segment
peekSegmentPtrs :: Ptr Segment -> IO Segment
peekSegmentPtrs p = return Segment
        `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 0) p)
        `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 4) p)
        `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 8) p)

-- Poke values to Segment
pokeSegmentPtrs :: Ptr Segment -> Segment -> IO ()
pokeSegmentPtrs p segment = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) p $ slength segment
        (\hsc_ptr -> pokeByteOff hsc_ptr 4) p $ stype segment
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) p $ sdata segment

-- Peek values from Acc Array
peekArrayPtrs :: Ptr AccArray -> IO AccArray
peekArrayPtrs p = return AccArray
        `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 0) p)
        `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 8) p)
        `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 16) p)

-- Poke values to Acc Array
pokeArrayPtrs :: Ptr AccArray -> AccArray -> IO ()
pokeArrayPtrs p accarray = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) p $ atype accarray
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) p $ ashape accarray
        (\hsc_ptr -> pokeByteOff hsc_ptr 16) p $ adata accarray

-- Converts from haskell array to accelerate array
toAccArrayInt :: (Shape sh) => sh -> Segment -> IO (A.Array sh Int32)
toAccArrayInt shp Segment {slength = len, stype = ty, sdata = ptr } = do
  arr <- fromPtr shp ((), castPtr ptr :: Ptr Int32)
  return arr

-- Converts from accelerate array to haskell array
fromAccArrayInt :: (Shape sh) => A.Array sh Int32 -> IO Segment
fromAccArrayInt arr =
  do let shp = A.arrayShape arr
         sz = A.arraySize shp
     ptr <- mallocArray sz
     toPtr arr ((), ptr)
     return $ Segment (Prelude.fromIntegral sz) (Prelude.fromIntegral 0) (castPtr ptr :: Ptr ())

-- Converts from haskell array to accelerate array
toAccArrayDbl :: (Shape sh) => sh -> Segment -> IO (A.Array sh Double)
toAccArrayDbl shp Segment {slength = len, stype = ty, sdata = ptr } = do
  arr <- fromPtr shp ((), castPtr ptr :: Ptr Double)
  return arr

-- Converts from accelerate array to haskell array
fromAccArrayDbl :: (Shape sh) => A.Array sh Double -> IO Segment
fromAccArrayDbl arr =
  do let shp = A.arrayShape arr
         sz = A.arraySize shp
     ptr <- mallocArray sz
     toPtr arr ((), ptr)
     return $ Segment (Prelude.fromIntegral sz) (Prelude.fromIntegral 1) (castPtr ptr :: Ptr ())

