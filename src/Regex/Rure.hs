module Regex.Rure ( compile
                  ) where

import qualified Data.ByteString    as BS
import           Foreign.ForeignPtr (castForeignPtr, newForeignPtr)
import           Foreign.Ptr        (castPtr, nullPtr)
import           Regex.Rure.FFI

-- | Compile with default flags
compile :: BS.ByteString -> IO (Either String RurePtr)
compile bs = do
    preErr <- rureErrorNew
    err <- castForeignPtr <$> newForeignPtr rureErrorFree (castPtr preErr)
    preOpt <- rureOptionsNew
    opt <- castForeignPtr <$> newForeignPtr rureOptionsFree (castPtr preOpt)
    BS.useAsCStringLen bs $ \(p, sz) -> do
        res <- rureCompile (castPtr p) (fromIntegral sz) rureDefaultFlags opt err
        if res == nullPtr
            then Left <$> rureErrorMessage err
            else Right . castForeignPtr <$> newForeignPtr rureFree (castPtr res)
