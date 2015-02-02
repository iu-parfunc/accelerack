module Data.Array.Accelerate.Capi where

import Data.Array.Accelerate.BackendKit.IRs

-- foreign export ccall entrypoint :: IO ()
foreign export ccall entrypoint :: Int -> IO Int

entrypoint :: Int -> IO Int
entrypoint x = do
  print "Hello from Haskell"
  return x
