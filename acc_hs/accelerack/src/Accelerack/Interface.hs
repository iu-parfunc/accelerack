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
foreign export ccall modify_array :: Ptr () -> IO ()
modify_array p = do
  a <- peekArrPtrs p
  add1Array (product $ arrShape a) $ arrData a
-}

{-
foreign export ccall modify_array :: Ptr () -> IO ()
modify_array p = pokeByteOff p 0 (0 :: CInt)
-}

foreign export ccall modify_array :: Ptr () -> IO ()
modify_array p = do
  psh <- peekByteOff p intSize 
  Segment tsh szsh psh' <- peek psh
  poke psh $ Segment (toEnum 0) szsh psh'

add1Array :: Int -> Type (Ptr ()) -> IO ()
add1Array len = \case
  Double p -> do
    let dp = castPtr p :: Ptr CDouble
    ds <- peekArray len dp
    pokeArray dp $ map (+1) ds


{-
foreign export ccall run_accelerate :: Ptr () -> CString -> FunPtr AllocFun -> IO ()
run_accelerate dp sp fp = do
  str      <- peekCString sp
  v :: Segment <- peek $ castPtr dp
  undefined
  -- infer type, shape
  -- arr <- mkAllocFun fp shlen shp typp
-}

