{-# Language TypeOperators #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE LambdaCase #-} 

module Example where
import Language.Haskell.Interpreter

import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.Marshal.Array

import Control.Applicative
import Control.Monad

import Data.Array.Accelerate as A hiding ((++), replicate, product)
import Data.Array.Accelerate.IO (fromPtr, toPtr)
import Data.Array.Accelerate.Interpreter as I

import qualified Data.List as L


-- C structure to store tuple/scalar information (one payload)
data Segment = Segment { 
    slength :: Int
  , stype :: Int
  , sdata :: Ptr Int
} deriving Show

-- C structure to store accelerate arrays information (one logical array)
data AccArray = AccArray { 
    atype :: Int
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

-- Exporting defined functions
foreign export ccall entrypoint :: Int -> IO Int
foreign export ccall gpu :: CString -> IO CString
foreign export ccall accelerateMap :: Ptr AccArray -> Int32 -> IO ()
foreign export ccall modifySegment :: Ptr Segment -> Int32 -> IO ()

-- Racket entry point function
entrypoint :: Int -> IO Int
entrypoint x = do
  print "Hello from Haskell"
  return x

parseEither x1 =
  case x1 of
    Left msg -> return (show msg)
    Right q  -> return q

-- Sample GPU computation function
gpu :: CString -> IO CString
gpu x = do
  print "Hello from Haskell"
  c_result <- peekCString x
  x1 <- runInterpreter $ setImports [ "Prelude", "Data.Array.Accelerate.Interpreter", "Data.Array.Accelerate"] >> eval c_result
  x <- parseEither x1
  new_str <- newCString x
  return new_str

-- Converts from haskell array to accelerate array
toAccArray :: Segment -> IO (A.Array DIM1 Int32)
toAccArray Segment {slength = len, stype = ty, sdata = ptr } = do
  arr <- fromPtr (Z :. len) ((), castPtr ptr :: Ptr Int32)
  return arr

-- Converts from accelerate array to haskell array
fromAccArray :: A.Array DIM1 Int32 -> IO Segment
fromAccArray arr =
  do let (Z :. sz) = A.arrayShape arr
     ptr <- mallocArray sz
     toPtr arr ((), ptr)
     return $ Segment (toEnum sz) (toEnum 0) (castPtr ptr :: Ptr Int)

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

-- Compute accelerate map
accmap :: A.Array DIM1 Int32 -> Int32 -> IO (A.Array DIM1 Int32)
accmap arr val = do
       let value = the (unit (constant val))
       return $ I.run $ A.map (+ value) (A.use arr)

-- Invoked from racket to access accelerate
accelerateMap p val = do
  AccArray atyp ashp adata <- peek p
  modifySegment adata val

-- modify segment - used by accelerateMap
modifySegment p val = do
  a0 <- peekSegmentPtrs p
  a1 <- toAccArray a0
  a2 <- accmap a1 val
  a3  <- fromAccArray a2
  p' <- malloc
  pokeSegmentPtrs p' a3
  Segment lseg tseg dseg <- peek p'
  poke p $ Segment lseg tseg dseg
  -- add1Array 10 dseg

-- add 1 to int array - currently not used
add1Array :: Int -> Ptr Int -> IO ()
add1Array len p =  do
    let ip = castPtr p :: Ptr Int
    is <- peekArray len ip
    pokeArray ip $ L.map (+1) is

