
module Accelerack.Run where

import Language.Haskell.TH
import Language.Haskell.TH.Ppr
import Language.Haskell.Interpreter
import Accelerack.Parse
import Accelerack.Gen

dataList :: IO (Either InterpreterError ())
dataList = runInterpreter $ do
  setImportsQ [("Prelude",Nothing),("Data.List",Just "L")]
  res1 <- interpret "L.map (+1) [1..4]" (as :: [Int])
  liftIO $ print res1
  res2 <- eval "L.map (+2) [3..8]"
  liftIO $ putStrLn res2

accelerack :: String -> IO ()
accelerack s = do
  b <- parsedBlock
  e <- runQ $ genBlock b
  let code = pprint e
  res <- runInterpreter $ do
    setImports
      [ "Data.Array.Accelerate"
      , "Data.Array.Accelerate.Interpreter"
      ]
    x <- eval code
    liftIO $ putStrLn x
  case res of
    Left err -> fail $ "Interpreter error: " ++ show err
    Right _  -> return ()
  where
  parsedBlock = case parseAccel s of
    Left err -> fail $ "Parse error: " ++ err
    Right b  -> return b

