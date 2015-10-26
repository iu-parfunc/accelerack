{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Accelerack.Interface where

import Foreign
import Foreign.C
import Foreign.Ptr

import Accelerack.Marshal
-- import Accelerack.Parse
import Control.Monad

-- foreign import ccall "dynamic" mkAllocFun :: FunPtr AllocFun -> AllocFun
-- 
-- type AllocFun = Int -> Ptr CInt -> Ptr () -> IO (Ptr ())

{-
parse :: String -> IO Exp
parse = either fail return . (runWithKnown known . parseExp <=< lexSexp)
-}

foreign export ccall print_array :: Ptr () -> IO ()
print_array p = do
  a <- peekArrPtrs p
  printArrPtrs a

{-
-- add1 to all array data
foreign export ccall modify_array :: Ptr () -> IO ()
modify_array p = do
  a <- peekArrPtrs p
  add1Array (product $ arrShape a) $ arrData a
-}

-- modify type tag
foreign export ccall modify_array :: Ptr () -> IO ()
modify_array p = do
  pdat <- peekByteOff p $ intSize + ptrSize
  Segment sz t pdat' <- peek pdat
  poke pdat $ Segment sz (toEnum 0) pdat'

{-
-- modify type length
foreign export ccall modify_array :: Ptr () -> IO ()
modify_array p = do
  pdat <- peekByteOff p $ intSize + ptrSize
  Segment sz t pdat' <- peek pdat
  poke pdat $ Segment (toEnum 0) t pdat'
-}

{-
-- modify type
foreign export ccall modify_array :: Ptr () -> IO ()
modify_array p = pokeByteOff p 0 (0 :: CInt)
-}

{-
-- modify shape length
foreign export ccall modify_array :: Ptr () -> IO ()
modify_array p = do
  psh <- peekByteOff p intSize 
  Segment szsh tsh psh' <- peek psh
  poke psh $ Segment (toEnum 0) tsh psh'
-}

add1Array :: Int -> Type (Ptr ()) -> IO ()
add1Array len = \case
  Double p -> do
    let dp = castPtr p :: Ptr CDouble
    ds <- peekArray len dp
    pokeArray dp $ map (+1) ds
  Int p -> do
    let ip = castPtr p :: Ptr CInt
    is <- peekArray len ip
    pokeArray ip $ map (+1) is
  Tuple ts -> forM_ ts $ add1Array len

{-
foreign export ccall run_accelerate :: Ptr () -> CString -> FunPtr AllocFun -> IO ()
run_accelerate dp sp fp = do
  str      <- peekCString sp
  v :: Segment <- peek $ castPtr dp
  undefined
  -- infer type, shape
  -- arr <- mkAllocFun fp shlen shp typp
-}

