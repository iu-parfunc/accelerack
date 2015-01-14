{-# LANGUAGE ForeignFunctionInterface    #-}

module Example where

foreign export ccall entrypoint :: IO ()

entrypoint :: IO ()
entrypoint = do
  print "Hello from Haskell"

  return ()