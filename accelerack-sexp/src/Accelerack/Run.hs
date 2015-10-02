
module Accelerack.Run where

import Language.Haskell.TH
import Language.Haskell.TH.Ppr
import Language.Haskell.Interpreter
import Accelerack.Parse
import Accelerack.Gen

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

