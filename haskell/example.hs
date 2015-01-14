{-# LANGUAGE ForeignFunctionInterface    #-}

module Example where

-- foreign export ccall entrypoint :: IO ()
foreign export ccall entrypoint :: Int -> IO Int

entrypoint :: Int -> IO Int
entrypoint x = do
  print "Hello from Haskell"
  return x
