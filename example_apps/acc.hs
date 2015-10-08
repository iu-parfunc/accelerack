{-# LANGUAGE ForeignFunctionInterface #-}
module Accelerac where
 
import Prelude

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Storable

-- impure function
foreign import ccall "getString" getStr :: CString -> IO ()

main :: IO()
main = do
  alloca $ \resultPtr -> do
  getStr resultPtr
  c_result <- peekCString resultPtr
  print c_result

