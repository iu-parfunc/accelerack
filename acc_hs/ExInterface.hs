{-# Language TypeOperators #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-} 
{-# LANGUAGE AllowAmbiguousTypes #-}

module ExInterface where

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

import ExUtils as E


-- Exporting defined functions
foreign export ccall entrypoint :: Int -> IO Int
foreign export ccall accelerateMap :: Ptr AccArray -> Ptr AccArray -> CInt -> CInt -> IO ()
foreign export ccall accelerateZipWith :: Ptr AccArray -> Ptr AccArray -> Ptr AccArray -> CInt -> IO ()
foreign export ccall accelerateFold :: Ptr AccArray -> Ptr AccArray -> CInt -> CInt -> IO ()

-- Racket entry point function
entrypoint :: Int -> IO Int
entrypoint x = do
  print "Hello from Haskell"
  return x


-- Invoked from racket to access accelerate
accelerateMap :: Ptr AccArray -> Ptr AccArray -> CInt -> CInt -> IO ()
accelerateMap p res val opr = do
  AccArray atyp ashp adata <- peek p
  AccArray rtyp rshp rdata <- peek res
  Segment s1ls s1ts s1da <- peek ashp
  Segment s2ls s2ts s2da <- peek rshp
  sh1 <- peekArray (Prelude.fromIntegral s1ls) (castPtr s1da :: Ptr CInt)
  sh2 <- peekArray (Prelude.fromIntegral s2ls) (castPtr s2da :: Ptr CInt)
  Segment ls ts ds <- peek adata
  E.modifySegmentMap adata sh1 rdata ts opr val


-- Invoked from racket to access accelerate
accelerateZipWith :: Ptr AccArray -> Ptr AccArray -> Ptr AccArray -> CInt -> IO ()
accelerateZipWith p1 p2 res bin = do
  AccArray atyp ashp adata <- peek p1
  AccArray btyp bshp bdata <- peek p2
  AccArray rtyp rshp rdata <- peek res
  Segment s1ls s1ts s1da <- peek ashp
  Segment s2ls s2ts s2da <- peek bshp
  Segment s3ls s3ts s3da <- peek rshp
  sh1 <- peekArray (Prelude.fromIntegral s1ls) (castPtr s1da :: Ptr CInt)
  sh2 <- peekArray (Prelude.fromIntegral s2ls) (castPtr s2da :: Ptr CInt)
  sh3 <- peekArray (Prelude.fromIntegral s3ls) (castPtr s3da :: Ptr CInt) 
  Segment ls ts ds <- peek adata
  Segment ls' ts' ds' <- peek bdata
  E.modifySegmentZipWith adata (Prelude.reverse sh1) bdata (Prelude.reverse sh2) rdata ts bin

-- Invoked from racket to access accelerate
accelerateFold :: Ptr AccArray -> Ptr AccArray -> CInt -> CInt -> IO ()
accelerateFold p res def val = do
  AccArray atyp ashp adata <- peek p
  AccArray rtyp rshp rdata <- peek res
  Segment s1ls s1ts s1da <- peek ashp
  Segment s2ls s2ts s2da <- peek rshp
  sh1 <- peekArray (Prelude.fromIntegral s1ls) (castPtr s1da :: Ptr CInt)
  sh2 <- peekArray (Prelude.fromIntegral s2ls) (castPtr s2da :: Ptr CInt)
  Segment ls ts ds <- peek adata
  E.modifySegmentFold adata (Prelude.reverse sh1) rdata ts def val


