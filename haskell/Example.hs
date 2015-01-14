{-# LINE 1 "Example.hsc" #-}
{-# LANGUAGE CPP                         #-}
{-# LINE 2 "Example.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface    #-}

module Example where

foreign export ccall entrypoint :: IO ()

entrypoint :: IO ()
entrypoint = do
  print "Hello from Haskell"

  return ()