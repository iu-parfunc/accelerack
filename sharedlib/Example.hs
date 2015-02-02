module Example where

-- foreign export ccall entrypoint :: IO ()
foreign export ccall entrypoint :: Int -> IO Int

entrypoint :: Int -> IO Int
entrypoint x = do
  print "Hello from Haskell"
  return x

data Const = MkInt Int
  deriving (Show,Read,Eq,Ord)

-- printConst :: StablePtr Const -> IO ()
-- printConst = print . derefStablePtr
