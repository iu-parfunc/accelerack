{-# Language TypeOperators #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# NOINLINE foo #-}

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

import Language.Haskell.TH
import Language.Haskell.TH.Ppr
import Language.Haskell.Interpreter
import Data.String.Utils as Ds
import Data.Either.Unwrap
import Data.List.Split
import System.IO.Unsafe

-- Exporting defined functions
foreign export ccall entrypoint :: Int -> IO Int
foreign export ccall runAcc :: CString -> Ptr AccArray -> Ptr AccArray -> IO ()

-- Racket entry point function
entrypoint :: Int -> IO Int
entrypoint x = do
  print "Hello from Haskell"
  return x

-- parseEither :: (Shape sh) => Either InterpreterError String -> A.Array sh Int
parseEither :: (Monad m, Show a) => Either a String -> m String
parseEither x1 =
  case x1 of
    Left msg -> return (show msg)
    Right q  -> return q

accArrayToString :: (Shape sh) => A.Array sh Int32 -> String
accArrayToString a1 = do
  x <- show a1
  return x


class AccAray a b where
  getAccArray :: [CInt] -> Segment -> IO (A.Array b a)

instance AccAray Int32 DIM0 where
  getAccArray shp segdata = do
    arr <- toAccArrayInt (dim0 shp) segdata
    return arr

instance AccAray Int32 DIM1 where
  getAccArray shp segdata = do
    arr <- toAccArrayInt (dim1 shp) segdata
    return arr

instance AccAray Int32 DIM2 where
  getAccArray shp segdata = do
    arr <- toAccArrayInt (dim2 shp) segdata
    return arr

instance AccAray Int32 DIM3 where
  getAccArray shp segdata = do
    arr <- toAccArrayInt (dim3 shp) segdata
    return arr

instance AccAray Int32 DIM4 where
  getAccArray shp segdata = do
    arr <- toAccArrayInt (dim4 shp) segdata
    return arr

instance AccAray Int32 DIM5 where
  getAccArray shp segdata = do
    arr <- toAccArrayInt (dim5 shp) segdata
    return arr

instance AccAray Int32 DIM6 where
  getAccArray shp segdata = do
    arr <- toAccArrayInt (dim6 shp) segdata
    return arr

instance AccAray Double DIM0 where
  getAccArray shp segdata = do
    arr <- toAccArrayDbl (dim0 shp) segdata
    return arr

instance AccAray Double DIM1 where
  getAccArray shp segdata = do
    arr <- toAccArrayDbl (dim1 shp) segdata
    return arr

instance AccAray Double DIM2 where
  getAccArray shp segdata = do
    arr <- toAccArrayDbl (dim2 shp) segdata
    return arr

instance AccAray Double DIM3 where
  getAccArray shp segdata = do
    arr <- toAccArrayDbl (dim3 shp) segdata
    return arr

instance AccAray Double DIM4 where
  getAccArray shp segdata = do
    arr <- toAccArrayDbl (dim4 shp) segdata
    return arr

instance AccAray Double DIM5 where
  getAccArray shp segdata = do
    arr <- toAccArrayDbl (dim5 shp) segdata
    return arr

instance AccAray Double DIM6 where
  getAccArray shp segdata = do
    arr <- toAccArrayDbl (dim6 shp) segdata
    return arr

getReferenceStr :: String -> Ptr AccArray -> IO String
getReferenceStr ref arr = do
  let ptrSize = 8 :: Int
  let value = (read ref) :: Int
  AccArray atyp ashp adata <- peek arr
  Segment s1 t1 dsg <- peek adata
  adata2 <- peekByteOff dsg (value * ptrSize)
  AccArray atyp' ashp' adata3 <- peek adata2
  -- Segment s1' t1' dsg'' <- peek adata3
  -- AccArray atyp' ashp' adata' <- peek dsg
  segdata <- E.peekSegmentPtrs adata3
  Segment s1ls' s1ts' s1da' <- peek adata3
  Segment s1ls s1ts s1da <- peek ashp'
  sh1 <- peekArray (Prelude.fromIntegral s1ls) (castPtr s1da :: Ptr CInt)
  a1 <- do
        case [(Prelude.fromIntegral s1ts'), (Prelude.length sh1)] of
          [0,0] -> do
             a1 <- getAccArray sh1 segdata :: IO (A.Array DIM0 Int32)
             str <- newCString (show a1)
             hstr <- peekCString str
             let nstr0 = replace "Array" "fromList" hstr
                 nstr1 = "(use (" ++ nstr0 ++ ":: Array DIM0 Int))"
             return nstr1
          [0,1] -> do
             a1 <- getAccArray sh1 segdata :: IO (A.Array DIM1 Int32)
             str <- newCString (show a1)
             hstr <- peekCString str
             let nstr0 = replace "Array" "fromList" hstr
                 nstr1 = "(use (" ++ nstr0 ++ ":: Array DIM1 Int))"
             return nstr1
          [0,2] -> do
             a1 <- getAccArray sh1 segdata :: IO (A.Array DIM2 Int32)
             str <- newCString (show a1)
             hstr <- peekCString str
             let nstr0 = replace "Array" "fromList" hstr
                 nstr1 = "(use (" ++ nstr0 ++ ":: Array DIM2 Int))"
             return nstr1
          [0,3] -> do
             a1 <- getAccArray sh1 segdata :: IO (A.Array DIM3 Int32)
             str <- newCString (show a1)
             hstr <- peekCString str
             let nstr0 = replace "Array" "fromList" hstr
                 nstr1 = "(use (" ++ nstr0 ++ ":: Array DIM3 Int))"
             return nstr1
          [0,4] -> do
             a1 <- getAccArray sh1 segdata :: IO (A.Array DIM4 Int32)
             str <- newCString (show a1)
             hstr <- peekCString str
             let nstr0 = replace "Array" "fromList" hstr
                 nstr1 = "(use (" ++ nstr0 ++ ":: Array DIM4 Int))"
             return nstr1
          [0,5] -> do
             a1 <- getAccArray sh1 segdata :: IO (A.Array DIM5 Int32)
             str <- newCString (show a1)
             hstr <- peekCString str
             let nstr0 = replace "Array" "fromList" hstr
                 nstr1 = "(use (" ++ nstr0 ++ ":: Array DIM5 Int))"
             return nstr1
          [0,6] -> do
             a1 <- getAccArray sh1 segdata :: IO (A.Array DIM6 Int32)
             str <- newCString (show a1)
             hstr <- peekCString str
             let nstr0 = replace "Array" "fromList" hstr
                 nstr1 = "(use (" ++ nstr0 ++ ":: Array DIM6 Int))"
             return nstr1
          [1,0] -> do
             a1 <- getAccArray sh1 segdata :: IO (A.Array DIM0 Double)
             str <- newCString (show a1)
             hstr <- peekCString str
             let nstr0 = replace "Array" "fromList" hstr
                 nstr1 = "(use (" ++ nstr0 ++ ":: Array DIM0 Double))"
             return nstr1
          [1,1] -> do
             a1 <- getAccArray sh1 segdata :: IO (A.Array DIM1 Double)
             str <- newCString (show a1)
             hstr <- peekCString str
             let nstr0 = replace "Array" "fromList" hstr
                 nstr1 = "(use (" ++ nstr0 ++ ":: Array DIM1 Double))"
             return nstr1
          [1,2] -> do
             a1 <- getAccArray sh1 segdata :: IO (A.Array DIM2 Double)
             str <- newCString (show a1)
             hstr <- peekCString str
             let nstr0 = replace "Array" "fromList" hstr
                 nstr1 = "(use (" ++ nstr0 ++ ":: Array DIM2 Double))"
             return nstr1
          [1,3] -> do
             a1 <- getAccArray sh1 segdata :: IO (A.Array DIM3 Double)
             str <- newCString (show a1)
             hstr <- peekCString str
             let nstr0 = replace "Array" "fromList" hstr
                 nstr1 = "(use (" ++ nstr0 ++ ":: Array DIM3 Double))"
             return nstr1
          [1,4] -> do
             a1 <- getAccArray sh1 segdata :: IO (A.Array DIM4 Double)
             str <- newCString (show a1)
             hstr <- peekCString str
             let nstr0 = replace "Array" "fromList" hstr
                 nstr1 = "(use (" ++ nstr0 ++ ":: Array DIM4 Double))"
             return nstr1
          [1,5] -> do
             a1 <- getAccArray sh1 segdata :: IO (A.Array DIM5 Double)
             str <- newCString (show a1)
             hstr <- peekCString str
             let nstr0 = replace "Array" "fromList" hstr
                 nstr1 = "(use (" ++ nstr0 ++ ":: Array DIM5 Double))"
             return nstr1
          [1,6] -> do
             a1 <- getAccArray sh1 segdata :: IO (A.Array DIM6 Double)
             str <- newCString (show a1)
             hstr <- peekCString str
             let nstr0 = replace "Array" "fromList" hstr
                 nstr1 = "(use (" ++ nstr0 ++ ":: Array DIM6 Double))"
             return nstr1
  return a1

appendNull :: Monad m => String -> m String
appendNull ref = do
  return $ ref ++ ""

checkNotNull :: [Char] -> Bool
checkNotNull str = if (str == "") then False else True

processReference :: String -> Ptr AccArray -> IO String
processReference ref arr = do
  let newRef = splitOneOf "() " ref -- :: [String]
      newRef2 = Prelude.filter (\x -> x /= "") newRef
  x <- (if ((head newRef2) == "id")
           then (getReferenceStr (last newRef2) arr)
           else if ((head newRef2) == "map")
                then (appendNull "Data.Array.Accelerate.map")
                else if ((head newRef2) == "zipwith")
                     then (appendNull "Data.Array.Accelerate.zipWith")
                     else if ((head newRef2) == "fold")
                          then (appendNull "Data.Array.Accelerate.fold")
                          else (appendNull ref))
  return x

appendReference :: [String] -> [String]
appendReference [] = []
appendReference (x1:x2:xs) = if (x1 == "(id") then (x1 ++ " " ++ x2) : (appendReference xs) else x1 : (appendReference (x2:xs))
appendReference _ = error "appendReference"

appendList :: [String] -> String
appendList [] = ""
appendList (x:xs) = x ++ " " ++ (appendList xs)

-- | Takes an SExpression in a string, takes payloads, and takes pre-allocated result space.
runAcc :: CString -> Ptr AccArray -> Ptr AccArray -> IO ()
runAcc x arr res = do
  str <- peekCString x
  let exp = splitWs str
  putStrLn $ "SExp-containing string received by Haskell: " ++ str
  let exp1 = appendReference exp
  -- putStrLn (show exp1)
  let exp2 = (L.map (\x1 -> let x2 = unsafePerformIO $ (processReference x1 arr) :: [Char]
                            in x2) exp1) :: [[Char]] --(processReference (last exp3) arr)
  -- putStrLn (show exp2)
  let exp3 = appendList exp2
  -- putStrLn (show exp3)
  x1 <- runInterpreter $ setImports ["Prelude", "Data.Array.Accelerate.Interpreter", "Data.Array.Accelerate"] >> eval ("run $ " ++ exp3)
  let values = fromRight x1
  let arr = (last (Ds.split " " values))
  AccArray atyp ashp adata <- peek res
  Segment s1ls s1ts s1da <- peek adata
  a1 <- do
        case (Prelude.fromIntegral s1ts) of
          0 -> do
               let arr' = (read arr) :: [Int]
               -- putStrLn (show arr')
               let a3 = L.map Prelude.fromIntegral arr' :: [CInt]
               pokeArray (castPtr s1da :: Ptr CInt) a3
          1 -> do
               let arr' = (read arr) :: [Double]
               -- putStrLn (show arr')
               let a3 = L.map Prelude.realToFrac arr' :: [CDouble]
               pokeArray (castPtr s1da :: Ptr CDouble) a3
          _ -> putStrLn "Dont Care Case !!!\n"
  new_str <- newCString values
  putStrLn (exp3 ++ "\n")
