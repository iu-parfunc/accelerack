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


-- Compute accelerate map
accMap :: (Shape dim, Elt e, IsNum e) => A.Array dim e -> e -> Int -> IO (A.Array dim e)
accMap arr val opr =
  case opr of
  0 -> do
       let value = the (unit (constant val))
       return $ I.run $ A.map (+ value) (A.use arr)
  1 -> do
       let value = the (unit (constant val))
       return $ I.run $ A.map (* value) (A.use arr)
  --2 -> do
    --   let value = the (unit (constant val))
      -- return $ I.run $ A.map (^ value) (A.use arr)


incr ref val = modifyIORef ref (+ val)

test ref f = do { val <- readIORef ref; return (f val) }

while test action = do
  val <- test
  if val then do {action;ExUtils.while test action}
         else return ()


-- modify segment - used by accelerateMap
modifySegmentMap :: Ptr Segment -> [CInt] -> Ptr Segment -> CInt -> CInt -> CInt -> IO ()
modifySegmentMap p sh1 res ts opr val =
  case (Prelude.fromIntegral ts) of
  0 -> do
       ia0 <- peekSegmentPtrs p
       ia3 <- processDimIntMap ia0 sh1 val opr
       p'  <- malloc
       pokeSegmentPtrs p' ia3
       Segment lseg tseg dseg <- peek p'
       poke res $ Segment lseg tseg dseg
  1 -> do
       da0 <- peekSegmentPtrs p
       da3 <- processDimDblMap da0 sh1 val opr
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
       ExUtils.while (test ref (< (Prelude.fromIntegral lseg)))
        (do
          offset' <- readIORef offset
          dsg  <- peekByteOff dseg offset'
          rdsg  <- peekByteOff rdseg offset'
          Segment ls ts ds <- peek dsg
          modifySegmentMap (castPtr dsg :: Ptr Segment) ((Prelude.head sh1) : []) (castPtr rdsg :: Ptr Segment) ts opr val
          incr offset ptrSize
          incr ref 1)
       -- poke res $ Segment lseg tseg dseg
  _ -> putStrLn "Dont Care Case !!!\n"

processDimIntMap :: Segment -> [CInt] -> CInt -> CInt -> IO Segment
processDimIntMap seg ls val opr = 
  case (Prelude.length ls) of
    0 -> do
         let shp = dim1 [Prelude.fromIntegral 1]
         -- print (Prelude.map Prelude.fromIntegral ls)
         a1 <- toAccArrayInt shp seg
         a2 <- accMap a1 (Prelude.fromIntegral val) (Prelude.fromIntegral opr)
         a3 <- fromAccArrayInt a2
         return a3
    1 -> do
         let shp = dim1 ls
         a1 <- toAccArrayInt shp seg
         a2 <- accMap a1 (Prelude.fromIntegral val) (Prelude.fromIntegral opr)
         a3 <- fromAccArrayInt a2
         return a3
    2 -> do
         let shp = dim2 ls
         a1 <- toAccArrayInt shp seg
         a2 <- accMap a1 (Prelude.fromIntegral val) (Prelude.fromIntegral opr)
         a3 <- fromAccArrayInt a2
         return a3
    3 -> do
         let shp = dim3 ls
         a1 <- toAccArrayInt shp seg
         a2 <- accMap a1 (Prelude.fromIntegral val) (Prelude.fromIntegral opr)
         a3 <- fromAccArrayInt a2
         return a3
    4 -> do
         let shp = dim4 ls
         a1 <- toAccArrayInt shp seg
         a2 <- accMap a1 (Prelude.fromIntegral val) (Prelude.fromIntegral opr)
         a3 <- fromAccArrayInt a2
         return a3
    5 -> do
         let shp = dim5 ls
         a1 <- toAccArrayInt shp seg
         a2 <- accMap a1 (Prelude.fromIntegral val) (Prelude.fromIntegral opr)
         a3 <- fromAccArrayInt a2
         return a3

processDimDblMap :: Segment -> [CInt] -> CInt -> CInt -> IO Segment
processDimDblMap seg ls val opr = 
  case (Prelude.length ls) of
    0 -> do
         let shp = dim1 [Prelude.fromIntegral 1]
         a1 <- toAccArrayDbl shp seg
         a2 <- accMap a1 (Prelude.realToFrac val) (Prelude.fromIntegral opr)
         a3 <- fromAccArrayDbl a2
         return a3
    1 -> do
         let shp = dim1 ls
         a1 <- toAccArrayDbl shp seg
         a2 <- accMap a1 (Prelude.realToFrac val) (Prelude.fromIntegral opr)
         a3 <- fromAccArrayDbl a2
         return a3
    2 -> do
         let shp = dim2 ls
         a1 <- toAccArrayDbl shp seg
         a2 <- accMap a1 (Prelude.realToFrac val) (Prelude.fromIntegral opr)
         a3 <- fromAccArrayDbl a2
         return a3
    3 -> do
         let shp = dim3 ls
         a1 <- toAccArrayDbl shp seg
         a2 <- accMap a1 (Prelude.realToFrac val) (Prelude.fromIntegral opr)
         a3 <- fromAccArrayDbl a2
         return a3
    4 -> do
         let shp = dim4 ls
         a1 <- toAccArrayDbl shp seg
         a2 <- accMap a1 (Prelude.realToFrac val) (Prelude.fromIntegral opr)
         a3 <- fromAccArrayDbl a2
         return a3
    5 -> do
         let shp = dim5 ls
         a1 <- toAccArrayDbl shp seg
         a2 <- accMap a1 (Prelude.realToFrac val) (Prelude.fromIntegral opr)
         a3 <- fromAccArrayDbl a2
         return a3


-- Compute accelerate zipwith
accZipWith :: (Shape dim, Elt e, IsNum e) => A.Array dim e -> A.Array dim e -> Int -> IO (A.Array dim e)
accZipWith arr1 arr2 bin = 
  case bin of
    0 -> do return $ I.run $ A.zipWith (+) (A.use arr1) (A.use arr2)
    1 -> do return $ I.run $ A.zipWith (-) (A.use arr1) (A.use arr2)
    2 -> do return $ I.run $ A.zipWith (*) (A.use arr1) (A.use arr2)

-- zipWithDiv :: (Shape dim) => A.Array dim Double -> A.Array dim Double -> Int -> IO (A.Array dim Double)
-- zipWithDiv arr1 arr2 bin = do return $ I.run $ A.zipWith (/) (A.use arr1) (A.use arr2)

-- modify segment - used by accelerateZipWith
modifySegmentZipWith :: Ptr Segment -> [CInt] -> Ptr Segment -> [CInt] -> Ptr Segment -> CInt -> CInt -> IO ()
modifySegmentZipWith p1 sh1 p2 sh2 res ts bin =
  case (Prelude.fromIntegral ts) of
    0 -> do
       ia0 <- peekSegmentPtrs p1
       ib0 <- peekSegmentPtrs p2
       ia3 <- processDimIntZipWith ia0 sh1 ib0 sh2 bin
       p'  <- malloc
       pokeSegmentPtrs p' ia3
       Segment lseg tseg dseg <- peek p'
       poke res $ Segment lseg tseg dseg
    1 -> do
       da0 <- peekSegmentPtrs p1
       db0 <- peekSegmentPtrs p2
       da3 <- processDimDblZipWith da0 sh1 db0 sh2 bin
       p'  <- malloc
       pokeSegmentPtrs p' da3
       Segment lseg tseg dseg <- peek p'
       poke res $ Segment lseg tseg dseg
    2 -> putStrLn "Dont Care for Boolean !!!\n"
    3 -> putStrLn "Dont Care for Tuple !!!\n"
    _ -> putStrLn "Dont Care Case !!!\n"

processDimIntZipWith :: Segment -> [CInt] -> Segment -> [CInt] -> CInt -> IO Segment
processDimIntZipWith seg1 ls1 seg2 ls2 bin = 
  case (Prelude.length ls1) of
    0 -> do
         let shp1 = dim1 [Prelude.fromIntegral 1]
             shp2 = if (Prelude.length ls2) == 0 then dim1 [Prelude.fromIntegral 1] else dim1 ls2
         a1 <- toAccArrayInt shp1 seg1
         b1 <- toAccArrayInt shp2 seg2
         a2 <- accZipWith a1 b1 (Prelude.fromIntegral bin)
         a3 <- fromAccArrayInt a2
         return a3
    1 -> do
         let shp1 = dim1 ls1
             shp2 = dim1 ls2
         a1 <- toAccArrayInt shp1 seg1
         b1 <- toAccArrayInt shp2 seg2
         a2 <- accZipWith a1 b1 (Prelude.fromIntegral bin)
         a3 <- fromAccArrayInt a2
         return a3
    2 -> do
         let shp1 = dim2 ls1
             shp2 = dim2 ls2
         a1 <- toAccArrayInt shp1 seg1
         b1 <- toAccArrayInt shp2 seg2
         a2 <- accZipWith a1 b1 (Prelude.fromIntegral bin)
         a3 <- fromAccArrayInt a2
         return a3
    3 -> do
         let shp1 = dim3 ls1
             shp2 = dim3 ls2
         a1 <- toAccArrayInt shp1 seg1
         b1 <- toAccArrayInt shp2 seg2
         a2 <- accZipWith a1 b1 (Prelude.fromIntegral bin)
         a3 <- fromAccArrayInt a2
         return a3
    4 -> do
         let shp1 = dim4 ls1
             shp2 = dim4 ls2
         a1 <- toAccArrayInt shp1 seg1
         b1 <- toAccArrayInt shp2 seg2
         a2 <- accZipWith a1 b1 (Prelude.fromIntegral bin)
         a3 <- fromAccArrayInt a2
         return a3
    5 -> do
         let shp1 = dim5 ls1
             shp2 = dim5 ls2
         a1 <- toAccArrayInt shp1 seg1
         b1 <- toAccArrayInt shp2 seg2
         a2 <- accZipWith a1 b1 (Prelude.fromIntegral bin)
         a3 <- fromAccArrayInt a2
         return a3

processDimDblZipWith :: Segment -> [CInt] -> Segment -> [CInt] -> CInt -> IO Segment
processDimDblZipWith seg1 ls1 seg2 ls2 bin = 
  case (Prelude.length ls1) of
    0 -> do
         let shp1 = dim1 [Prelude.fromIntegral 1]
             shp2 = if (Prelude.length ls2) == 0 then dim1 [Prelude.fromIntegral 1] else dim1 ls2
         a1 <- toAccArrayDbl shp1 seg1
         b1 <- toAccArrayDbl shp2 seg2
         a2 <- accZipWith a1 b1 (Prelude.fromIntegral bin)
         a3 <- fromAccArrayDbl a2
         return a3
    1 -> do
         let shp1 = dim1 ls1
             shp2 = dim1 ls2
         a1 <- toAccArrayDbl shp1 seg1
         b1 <- toAccArrayDbl shp2 seg2
         a2 <- accZipWith a1 b1 (Prelude.fromIntegral bin)
         a3 <- fromAccArrayDbl a2
         return a3
    2 -> do
         let shp1 = dim2 ls1
             shp2 = dim2 ls2
         a1 <- toAccArrayDbl shp1 seg1
         b1 <- toAccArrayDbl shp2 seg2
         a2 <- accZipWith a1 b1 (Prelude.fromIntegral bin)
         a3 <- fromAccArrayDbl a2
         return a3
    3 -> do
         let shp1 = dim3 ls1
             shp2 = dim3 ls2
         a1 <- toAccArrayDbl shp1 seg1
         b1 <- toAccArrayDbl shp2 seg2
         a2 <- accZipWith a1 b1 (Prelude.fromIntegral bin)
         a3 <- fromAccArrayDbl a2
         return a3
    4 -> do
         let shp1 = dim4 ls1
             shp2 = dim4 ls2
         a1 <- toAccArrayDbl shp1 seg1
         b1 <- toAccArrayDbl shp2 seg2
         a2 <- accZipWith a1 b1 (Prelude.fromIntegral bin)
         a3 <- fromAccArrayDbl a2
         return a3
    5 -> do
         let shp1 = dim5 ls1
             shp2 = dim5 ls2
         a1 <- toAccArrayDbl shp1 seg1
         b1 <- toAccArrayDbl shp2 seg2
         a2 <- accZipWith a1 b1 (Prelude.fromIntegral bin)
         a3 <- fromAccArrayDbl a2
         return a3


-- Compute accelerate map
accFold :: (Shape dim, Elt e, IsNum e, Eq e) => A.Array (dim :. Int) e -> e -> Int -> IO (A.Array dim e)
accFold arr def bin = 
       case bin of
       0 -> do
            let value = the (unit (constant def)) 
            return $ I.run $ A.fold (+) value (A.use arr)
       1 -> do
            let value = the (unit (constant def)) 
            return $ I.run $ A.fold (-) value (A.use arr)
       2 -> do
            let value = the (unit (constant def)) 
            return $ I.run $ A.fold (*) value (A.use arr)


-- modify segment - used by accelerateMap
modifySegmentFold :: Ptr Segment -> [CInt] -> Ptr Segment -> CInt -> CInt -> CInt -> IO ()
modifySegmentFold p sh1 res ts def val =
  case (Prelude.fromIntegral ts) of
  0 -> do
       ia0 <- peekSegmentPtrs p
       ia3 <- processDimIntFold ia0 sh1 def val
       p'  <- malloc
       pokeSegmentPtrs p' ia3
       Segment lseg tseg dseg <- peek p'
       poke res $ Segment lseg tseg dseg
  1 -> do
       da0 <- peekSegmentPtrs p
       da3 <- processDimDblFold da0 sh1 def val
       p'  <- malloc
       pokeSegmentPtrs p' da3
       Segment lseg tseg dseg <- peek p'
       poke res $ Segment lseg tseg dseg
  2 -> putStrLn "Dont Care for Boolean !!!\n"
  3 -> putStrLn "Dont Care for Tuple !!!\n"
  _ -> putStrLn "Dont Care Case !!!\n"

processDimIntFold :: Segment -> [CInt] -> CInt -> CInt -> IO Segment
processDimIntFold seg ls def val = 
  case (Prelude.length ls) of
    0 -> do
         let shp = dim1 [Prelude.fromIntegral 1]
         a1 <- toAccArrayInt shp seg
         a2 <- accFold a1 (Prelude.fromIntegral def) (Prelude.fromIntegral val)
         a3 <- fromAccArrayInt a2
         return a3
    1 -> do
         let shp = dim1 ls
         a1 <- toAccArrayInt shp seg
         a2 <- accFold a1 (Prelude.fromIntegral def) (Prelude.fromIntegral val)
         a3 <- fromAccArrayInt a2
         return a3
    2 -> do
         let shp = dim2 ls
         a1 <- toAccArrayInt shp seg
         a2 <- accFold a1 (Prelude.fromIntegral def) (Prelude.fromIntegral val)
         a3 <- fromAccArrayInt a2
         return a3
    3 -> do
         let shp = dim3 ls
         a1 <- toAccArrayInt shp seg
         a2 <- accFold a1 (Prelude.fromIntegral def) (Prelude.fromIntegral val)
         a3 <- fromAccArrayInt a2
         return a3
    4 -> do
         let shp = dim4 ls
         a1 <- toAccArrayInt shp seg
         a2 <- accFold a1 (Prelude.fromIntegral def) (Prelude.fromIntegral val)
         a3 <- fromAccArrayInt a2
         return a3
    5 -> do
         let shp = dim5 ls
         a1 <- toAccArrayInt shp seg
         a2 <- accFold a1 (Prelude.fromIntegral def) (Prelude.fromIntegral val)
         a3 <- fromAccArrayInt a2
         return a3

processDimDblFold :: Segment -> [CInt] -> CInt -> CInt -> IO Segment
processDimDblFold seg ls def val = 
  case (Prelude.length ls) of
    0 -> do
         let shp = dim1 [Prelude.fromIntegral 1]
         a1 <- toAccArrayDbl shp seg
         a2 <- accFold a1 (Prelude.realToFrac def) (Prelude.fromIntegral val)
         a3 <- fromAccArrayDbl a2
         return a3
    1 -> do
         let shp = dim1 ls
         a1 <- toAccArrayDbl shp seg
         a2 <- accFold a1 (Prelude.realToFrac def) (Prelude.fromIntegral val)
         a3 <- fromAccArrayDbl a2
         return a3
    2 -> do
         let shp = dim2 ls
         a1 <- toAccArrayDbl shp seg
         a2 <- accFold a1 (Prelude.realToFrac def) (Prelude.fromIntegral val)
         a3 <- fromAccArrayDbl a2
         return a3
    3 -> do
         let shp = dim3 ls
         a1 <- toAccArrayDbl shp seg
         a2 <- accFold a1 (Prelude.realToFrac def) (Prelude.fromIntegral val)
         a3 <- fromAccArrayDbl a2
         return a3
    4 -> do
         let shp = dim4 ls
         a1 <- toAccArrayDbl shp seg
         a2 <- accFold a1 (Prelude.realToFrac def) (Prelude.fromIntegral val)
         a3 <- fromAccArrayDbl a2
         return a3
    5 -> do
         let shp = dim5 ls
         a1 <- toAccArrayDbl shp seg
         a2 <- accFold a1 (Prelude.realToFrac def) (Prelude.fromIntegral val)
         a3 <- fromAccArrayDbl a2
         return a3


{-
getShape :: (Shape shp) => [CInt] -> shp
getShape ls = 
  case (Prelude.length ls) of
    0 -> mkDim [Prelude.fromIntegral 1] :: DIM1
    1 -> mkDim ls :: DIM1
    2 -> mkDim ls :: DIM2
    3 -> mkDim ls :: DIM3
   -- 4 -> mkDim ls
   -- 5 -> mkDim ls
   -- 6 -> mkDim ls
   -- 7 -> mkDim ls
   -- 8 -> mkDim ls
   -- 9 -> mkDim ls
-}
-- repN :: (Shape sh, Elt e) => Int -> Acc (Array sh e) -> Acc (Array (sh:.Int) e)
-- repN n a = Prelude.replicate (constant $ Any :. n) a

mkDim :: Shape sh => [CInt] -> sh
mkDim ls = listToShape (Prelude.map Prelude.fromIntegral ls)

dim0 :: [CInt] -> DIM0
dim0 ls = mkDim ls

dim1 :: [CInt] -> DIM1
dim1 ls = mkDim ls

dim2 :: [CInt] -> DIM2
dim2 ls = mkDim ls

dim3 :: [CInt] -> DIM3
dim3 ls = mkDim ls

dim4 :: [CInt] -> DIM4
dim4 ls = mkDim ls

dim5 :: [CInt] -> DIM5
dim5 ls = mkDim ls

dim6 :: [CInt] -> DIM6
dim6 ls = mkDim ls

dim7 :: [CInt] -> DIM7
dim7 ls = mkDim ls

dim8 :: [CInt] -> DIM8
dim8 ls = mkDim ls

dim9 :: [CInt] -> DIM9
dim9 ls = mkDim ls


square :: (Elt e, IsNum e, Shape dim) => Array dim e -> IO (Array dim e)
square xs = do 
    return $ I.run $ A.zipWith (*) (use xs) (use xs)

-- add 1 to int array - currently not used
add1Array :: Int -> Ptr Int -> IO ()
add1Array len p =  do
    let ip = castPtr p :: Ptr Int
    is <- peekArray len ip
    pokeArray ip $ L.map (+1) is

