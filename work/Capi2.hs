{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}

module HsFoo where

import Foreign
import Foreign.C

import Control.Applicative
import Control.Monad

data Foo = Foo { 
    datatype :: CString
  , shape :: Ptr Int
  , accdata :: Ptr Double
} deriving Show

instance Storable Foo where
    sizeOf    _ = (12)
    alignment _ = alignment (undefined :: CString)

    poke p foo = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) p $ datatype foo
        (\hsc_ptr -> pokeByteOff hsc_ptr 4) p $ shape foo
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) p $ accdata foo

    peek p = return Foo
              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 0) p)
              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 4) p)
              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 8) p)

foreign import ccall getA :: Ptr Foo -> IO CString
getAvalue :: Ptr Foo -> IO CString
getAvalue f = do
  newA <- getA f
  return newA

foreign import ccall getB :: Ptr Foo -> IO (Ptr Int)
getBvalue :: Ptr Foo -> IO (Ptr Int)
getBvalue f = do
  newB <- getB f
  return newB

foreign import ccall getC :: Ptr Foo -> IO (Ptr Double)
getCvalue :: Ptr Foo -> IO (Ptr Double)
getCvalue f = do
  newC <- getC f
  return newC

foreign export ccall "getAvalue" getAvalue :: Ptr Foo -> IO CString
foreign export ccall "getBvalue" getBvalue :: Ptr Foo -> IO (Ptr Int)
foreign export ccall "getCvalue" getCvalue :: Ptr Foo -> IO (Ptr Double)

