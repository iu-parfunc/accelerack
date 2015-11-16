{-# Language TypeOperators #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE LambdaCase #-} 

module Example where

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

-- Exporting defined functions
foreign export ccall entrypoint :: Int -> IO Int
foreign export ccall accelerateMap :: Ptr AccArray -> Ptr AccArray -> CInt -> IO ()
foreign export ccall modifySegment :: Ptr Segment -> Ptr Segment -> CInt -> CInt -> IO ()
foreign export ccall accelerateZipWith :: Ptr AccArray -> Ptr AccArray -> Ptr AccArray -> CInt -> IO ()
-- foreign export ccall gpu :: CString -> IO CString

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
{-
gpu :: CString -> IO CString
gpu x = do
  print "Hello from Haskell"
  c_result <- peekCString x
  x1 <- runInterpreter $ setImports [ "Prelude", "Data.Array.Accelerate.Interpreter", "Data.Array.Accelerate"] >> eval c_result
  x <- parseEither x1
  new_str <- newCString x
  return new_str
-}

-- Converts from haskell array to accelerate array
toAccArrayInt :: Segment -> IO (A.Array DIM1 Int32)
toAccArrayInt Segment {slength = len, stype = ty, sdata = ptr } = do
  arr <- fromPtr (Z :. Prelude.fromIntegral len) ((), castPtr ptr :: Ptr Int32)
  return arr

-- Converts from accelerate array to haskell array
fromAccArrayInt :: A.Array DIM1 Int32 -> IO Segment
fromAccArrayInt arr =
  do let (Z :. sz) = A.arrayShape arr
     ptr <- mallocArray sz
     toPtr arr ((), ptr)
     return $ Segment (Prelude.fromIntegral sz) (Prelude.fromIntegral 0) (castPtr ptr :: Ptr ())

-- Converts from haskell array to accelerate array
toAccArrayDbl :: Segment -> IO (A.Array DIM1 Double)
toAccArrayDbl Segment {slength = len, stype = ty, sdata = ptr } = do
  arr <- fromPtr (Z :. Prelude.fromIntegral len) ((), castPtr ptr :: Ptr Double)
  return arr

-- Converts from accelerate array to haskell array
fromAccArrayDbl :: A.Array DIM1 Double -> IO Segment
fromAccArrayDbl arr =
  do let (Z :. sz) = A.arrayShape arr
     ptr <- mallocArray sz
     toPtr arr ((), ptr)
     return $ Segment (Prelude.fromIntegral sz) (Prelude.fromIntegral 1) (castPtr ptr :: Ptr ())

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
accmapInt :: A.Array DIM1 Int32 -> Int32 -> IO (A.Array DIM1 Int32)
accmapInt arr val = do
       let value = the (unit (constant val))
       return $ I.run $ A.map (+ value) (A.use arr)

accmapDbl :: A.Array DIM1 Double -> Double -> IO (A.Array DIM1 Double)
accmapDbl arr val = do
       let value = the (unit (constant val))
       -- print (show $ I.run $ A.map (+ value) (A.use arr))
       return $ I.run $ A.map (+ value) (A.use arr)

-- Invoked from racket to access accelerate
accelerateMap :: Ptr AccArray -> Ptr AccArray -> CInt -> IO ()
accelerateMap p res val = do
  AccArray atyp ashp adata <- peek p
  AccArray rtyp rshp rdata <- peek res
  Segment ls ts ds <- peek adata
  modifySegment adata rdata ts val


incr ref val = modifyIORef ref (+ val)

test ref f = do { val <- readIORef ref; return (f val) }

while test action = do
  val <- test
  if val then do {action;Example.while test action}
         else return ()


-- modify segment - used by accelerateMap
modifySegment :: Ptr Segment -> Ptr Segment -> CInt -> CInt -> IO ()
modifySegment p res ts val =
  case (Prelude.fromIntegral ts) of
  0 -> do
       -- let pt = castPtr ds :: Ptr Int
       ia0 <- peekSegmentPtrs p
       ia1 <- toAccArrayInt ia0
       ia2 <- accmapInt ia1 (Prelude.fromIntegral val)
       ia3 <- fromAccArrayInt ia2
       p'  <- malloc
       pokeSegmentPtrs p' ia3
       Segment lseg tseg dseg <- peek p'
       poke res $ Segment lseg tseg dseg
  1 -> do
       da0 <- peekSegmentPtrs p
       da1 <- toAccArrayDbl da0
       da2 <- accmapDbl da1 (Prelude.realToFrac val)
       da3 <- fromAccArrayDbl da2
       p'  <- malloc
       pokeSegmentPtrs p' da3
       Segment lseg tseg dseg <- peek p'
       poke res $ Segment lseg tseg dseg
  2 -> putStrLn "Dont Care for Boolean !!!\n"
  3 -> do
       Segment lseg tseg dseg <- peek p
       Segment rlseg rtseg rdseg <- peek res
       ref <- newIORef 0
       offset <- newIORef 0
       let ptrSize = 8
       Example.while (test ref (< (Prelude.fromIntegral lseg)))
        (do
          offset' <- readIORef offset
          dsg  <- peekByteOff dseg offset'
          rdsg  <- peekByteOff rdseg offset'
          Segment ls ts ds <- peek dsg
          modifySegment (castPtr dsg :: Ptr Segment) (castPtr rdsg :: Ptr Segment) ts val
          incr offset ptrSize
          incr ref 1)
       -- poke res $ Segment lseg tseg dseg
  _ -> putStrLn "Dont Care Case !!!\n"


-- Compute accelerate zipwith
zipWithInt :: A.Array DIM1 Int32 -> A.Array DIM1 Int32 -> Int -> IO (A.Array DIM1 Int32)
zipWithInt arr1 arr2 bin = 
  case bin of
    0 -> do return $ I.run $ A.zipWith (+) (A.use arr1) (A.use arr2)
    1 -> do return $ I.run $ A.zipWith (-) (A.use arr1) (A.use arr2)
    2 -> do return $ I.run $ A.zipWith (*) (A.use arr1) (A.use arr2)

zipWithDbl :: A.Array DIM1 Double -> A.Array DIM1 Double -> Int -> IO (A.Array DIM1 Double)
zipWithDbl arr1 arr2 bin = 
  case bin of
    0 -> do return $ I.run $ A.zipWith (+) (A.use arr1) (A.use arr2)
    1 -> do return $ I.run $ A.zipWith (-) (A.use arr1) (A.use arr2)
    2 -> do return $ I.run $ A.zipWith (*) (A.use arr1) (A.use arr2)
    3 -> do return $ I.run $ A.zipWith (/) (A.use arr1) (A.use arr2)

-- Invoked from racket to access accelerate
accelerateZipWith :: Ptr AccArray -> Ptr AccArray -> Ptr AccArray -> CInt -> IO ()
accelerateZipWith p1 p2 res bin = do
  AccArray atyp ashp adata <- peek p1
  AccArray btyp bshp bdata <- peek p2
  AccArray rtyp rshp rdata <- peek res
  Segment ls ts ds <- peek adata
  Segment ls' ts' ds' <- peek bdata
  modifySegment' adata bdata rdata ts bin


-- modify segment - used by accelerateZipWith
modifySegment' :: Ptr Segment -> Ptr Segment -> Ptr Segment -> CInt -> CInt -> IO ()
modifySegment' p1 p2 res ts bin =
  case (Prelude.fromIntegral ts) of
    0 -> do
       ia0 <- peekSegmentPtrs p1
       ia1 <- toAccArrayInt ia0
       ib0 <- peekSegmentPtrs p2
       ib1 <- toAccArrayInt ib0
       ia2 <- zipWithInt ia1 ib1 (Prelude.fromIntegral bin)
       ia3 <- fromAccArrayInt ia2
       p'  <- malloc
       pokeSegmentPtrs p' ia3
       Segment lseg tseg dseg <- peek p'
       poke res $ Segment lseg tseg dseg
    1 -> do
       da0 <- peekSegmentPtrs p1
       da1 <- toAccArrayDbl da0
       db0 <- peekSegmentPtrs p2
       db1 <- toAccArrayDbl db0
       da2 <- zipWithDbl da1 db1 (Prelude.fromIntegral bin)
       da3 <- fromAccArrayDbl da2
       p'  <- malloc
       pokeSegmentPtrs p' da3
       Segment lseg tseg dseg <- peek p'
       poke res $ Segment lseg tseg dseg
    2 -> putStrLn "Dont Care for Boolean !!!\n"
    3 -> putStrLn "Dont Care for Tuple !!!\n"
    _ -> putStrLn "Dont Care Case !!!\n"


-- add 1 to int array - currently not used
add1Array :: Int -> Ptr Int -> IO ()
add1Array len p =  do
    let ip = castPtr p :: Ptr Int
    is <- peekArray len ip
    pokeArray ip $ L.map (+1) is

