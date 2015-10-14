module Example where
import Foreign.C.String
--import System.Eval

-- foreign export ccall entrypoint :: IO ()
foreign export ccall entrypoint :: Int -> IO Int
foreign export ccall gpu :: CString -> IO CString

--imports :: [Import]
--imports =
--  [ "qualified Data.Array.Accelerate.Interpreter as I"
--  , "qualified Data.Array.Accelerate as A"
--  ]

gpu :: CString -> IO CString
gpu x = do
  print "Hello from Haskell"
  c_result <- peekCString x
  --output <- eval c_result imports
  new_str <- newCString "Haskell"
  return new_str

entrypoint :: Int -> IO Int
entrypoint x = do
  print "Hello from Haskell"
  return x
