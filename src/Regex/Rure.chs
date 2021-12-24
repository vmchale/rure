{-# LANGUAGE FlexibleContexts #-}

module Regex.Rure ( compile
                  , isMatch
                  , find
                  ) where

import Data.Coerce (coerce)
import qualified Data.ByteString    as BS
import           Foreign.C.Types    (CSize)
import           Foreign.ForeignPtr (castForeignPtr, newForeignPtr)
import           Foreign.Ptr        (castPtr, nullPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import           Regex.Rure.FFI

-- TODO: not this
#undef __arm64__
#define __arm__
#include <rure.h>

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

find :: RurePtr 
     -> BS.ByteString -- ^ Unicode
     -> CSize -- ^ Start
     -> IO (Maybe RureMatch)
find rePtr haystack start =
    allocaBytes {# sizeof rure_match #} $ \matchPtr -> do
        res <- BS.useAsCStringLen haystack $ \(p, sz) ->
            rureFind rePtr (castPtr p) (fromIntegral sz) start matchPtr
        if res
            then fmap Just $
                mkRureMatch
                    <$> {# get rure_match->start #} matchPtr
                    <*> {# get rure_match->end #} matchPtr
            else pure Nothing
    where mkRureMatch start' end = RureMatch (coerce start') (coerce end)

isMatch :: RurePtr
        -> BS.ByteString -- ^ Unicode
        -> CSize -- ^ Start
        -> IO Bool
isMatch rePtr haystack start =
    BS.useAsCStringLen haystack $ \(p, sz) ->
        rureIsMatch rePtr (castPtr p) (fromIntegral sz) start
