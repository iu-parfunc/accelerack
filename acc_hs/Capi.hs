{-# Language TypeOperators #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}

module Example where
import Foreign.C.String
import System.Eval
import Data.Array.Accelerate
import Foreign
import Foreign.C
import Foreign.C.Types

import Control.Applicative
import Control.Monad

data Foo = Foo { 
    listlength :: Int
  , datatype :: CString
  , dimension :: Ptr Int
  , accdata :: Ptr Double
} deriving Show

instance Storable Foo where
    sizeOf    _ = (16)
    alignment _ = alignment (undefined :: CString)

    poke p foo = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) p $ listlength foo
        (\hsc_ptr -> pokeByteOff hsc_ptr 4) p $ datatype foo
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) p $ dimension foo
        (\hsc_ptr -> pokeByteOff hsc_ptr 12) p $ accdata foo

    peek p = return Foo
              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 0) p)
              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 4) p)
              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 8) p)
              `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 12) p)

-- foreign export ccall entrypoint :: IO ()
foreign export ccall entrypoint :: Int -> IO Int
foreign export ccall gpu :: CString -> IO CString

foreign import ccall "dynamic"
  typePtrFuncToFunc:: FunPtr (Ptr Foo -> IO CString) -> (Ptr Foo -> IO CString)
foreign import ccall "dynamic"
  dimensionPtrFuncToFunc:: FunPtr (Ptr Foo -> IO (Ptr Int)) -> (Ptr Foo -> IO (Ptr Int))
foreign import ccall "dynamic"
  dataPtrFuncToFunc:: FunPtr (Ptr Foo -> IO (Ptr Double)) -> (Ptr Foo -> IO (Ptr Double))

foreign export ccall getTypeHs :: Ptr Foo -> FunPtr (Ptr Foo -> IO CString) -> IO CString
foreign export ccall getDimensionHs :: Ptr Foo -> FunPtr (Ptr Foo -> IO (Ptr Int)) -> IO (Ptr Int)
foreign export ccall getDataHs :: Ptr Foo -> FunPtr (Ptr Foo -> IO (Ptr Double)) -> IO (Ptr Double)

imports :: [Import]
imports =
  [ "qualified Data.Array.Accelerate.Interpreter as I"
  , "qualified Data.Array.Accelerate as A"
  ]

gpu :: CString -> IO CString
gpu x = do
  print "Hello from Haskell"
  c_result <- peekCString x
  x1 <- eval c_result imports
  print (x1::Maybe String)
  new_str <- newCString (fromMaybe "" (x1:: Maybe String))  
  return new_str

entrypoint :: Int -> IO Int
entrypoint x = do
  print "Hello from Haskell"
  return x

getTypeHs :: Ptr Foo -> FunPtr (Ptr Foo -> IO CString) -> IO CString
getTypeHs cptr getTypeRkt = do
  newType <- typePtrFuncToFunc getTypeRkt cptr
  return newType

getDimensionHs :: Ptr Foo -> FunPtr (Ptr Foo -> IO (Ptr Int)) -> IO (Ptr Int)
getDimensionHs cptr getDimensionRkt = do
  newType <- dimensionPtrFuncToFunc getDimensionRkt cptr
  return newType

getDataHs :: Ptr Foo -> FunPtr (Ptr Foo -> IO (Ptr Double)) -> IO (Ptr Double)
getDataHs cptr getDataRkt = do
  newType <- dataPtrFuncToFunc getDataRkt cptr
  return newType
