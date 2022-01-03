module Regex.Rure ( -- * Higher-level functions
                    hsMatches
                  , hsIsMatch
                  , hsSetIsMatch
                  , hsFind
                  , hsSetMatches
                  -- * Stateful, functions in 'IO'
                  , compile
                  , compileSet
                  , isMatch
                  , setIsMatch
                  , setMatches
                  , find
                  , matches
                  , mkIter
                  -- * Types
                  , RureMatch (..)
                  -- ** Pointer types (stateful, 'IO'-based API)
                  , RurePtr
                  , RureIterPtr
                  , RureSetPtr
                  -- * Options/flags
                  , RureFlags
                  , rureFlagCaseI
                  , rureFlagMulti
                  , rureFlagDotNL
                  , rureFlagSwapGreed
                  , rureFlagSpace
                  , rureFlagUnicode
                  , rureDefaultFlags
                  ) where

import Data.Coerce (coerce)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Foldable (traverse_)
import Foreign.C.Types (CSize)
import Foreign.ForeignPtr (castForeignPtr, newForeignPtr, touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (castPtr, nullPtr, Ptr)
import Foreign.Storable (sizeOf)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (peekArray, pokeArray)
import Regex.Rure.FFI
import System.IO.Unsafe (unsafePerformIO)

#undef __arm64__
#define __arm__
#include <rure.h>

mkIter :: RurePtr -> IO RureIterPtr
mkIter rePtr =
    castForeignPtr <$> (newForeignPtr rureIterFree . castPtr =<< rureIterNew rePtr)

compileSet :: RureFlags -> [BS.ByteString] -> IO (Either String RureSetPtr)
compileSet flags bss = do
    preErr <- rureErrorNew
    err <- castForeignPtr <$> newForeignPtr rureErrorFree (castPtr preErr)
    preOpt <- rureOptionsNew
    opt <- castForeignPtr <$> newForeignPtr rureOptionsFree (castPtr preOpt)
    allocaBytes lBytes $ \bPtrs ->
        allocaBytes lBytes $ \szs -> do
            pokeArray bPtrs (fmap unsafeForeignPtrToPtr ps)
            pokeArray szs (fromIntegral <$> ss)
            res <- rureCompileSet (castPtr bPtrs) szs (fromIntegral l) flags opt err
            traverse_ touchForeignPtr ps
            if res == nullPtr
                then Left <$> rureErrorMessage err
                else Right . castForeignPtr <$> newForeignPtr rureSetFree (castPtr res)
    where l = length bss
          lBytes = l * sizeOf (undefined :: Ptr a)
          rip (BS.BS psϵ lϵ) = (psϵ, lϵ)
          (ps, ss) = unzip (fmap rip bss)

compile :: RureFlags -> BS.ByteString -> IO (Either String RurePtr)
compile flags re = do
    preErr <- rureErrorNew
    err <- castForeignPtr <$> newForeignPtr rureErrorFree (castPtr preErr)
    preOpt <- rureOptionsNew
    opt <- castForeignPtr <$> newForeignPtr rureOptionsFree (castPtr preOpt)
    BS.unsafeUseAsCStringLen re $ \(p, sz) -> do
        res <- rureCompile (castPtr p) (fromIntegral sz) flags opt err
        if res == nullPtr
            then Left <$> rureErrorMessage err
            else Right . castForeignPtr <$> newForeignPtr rureFree (castPtr res)

{-# NOINLINE hsMatches #-}
hsMatches :: RureFlags
          -> BS.ByteString -- ^ Regex
          -> BS.ByteString -- ^ Haystack (unicode)
          -> Either String [RureMatch]
hsMatches flags re haystack = unsafePerformIO $ do
    rePtr <- compile flags re
    case rePtr of
        Left err -> pure (Left err)
        Right rp -> Right <$> ((\riPtr -> matches riPtr haystack) =<< mkIter rp)

matches :: RureIterPtr
        -> BS.ByteString
        -> IO [RureMatch]
matches reIPtr haystack = do
    res <- iterNext reIPtr haystack
    case res of
        Nothing -> pure []
        Just m -> (m :) <$> matches reIPtr haystack

iterNext :: RureIterPtr
         -> BS.ByteString
         -> IO (Maybe RureMatch)
iterNext reIPtr haystack =
    allocaBytes {# sizeof rure_match #} $ \matchPtr -> do
        res <- BS.unsafeUseAsCStringLen haystack $ \(p, sz) ->
            rureIterNext reIPtr (castPtr p) (fromIntegral sz) matchPtr
        if res
            then Just <$> rureMatchFromPtr matchPtr
            else pure Nothing

rureMatchFromPtr :: Ptr RureMatch -> IO RureMatch
rureMatchFromPtr matchPtr =
    RureMatch
        <$> fmap coerce ({# get rure_match->start #} matchPtr)
        <*> fmap coerce ({# get rure_match->end #} matchPtr)

{-# NOINLINE hsFind #-}
hsFind :: RureFlags
       -> BS.ByteString -- ^ Regex
       -> BS.ByteString -- ^ Haystack
       -> Either String (Maybe (RureMatch))
hsFind flags re haystack = unsafePerformIO $ do
    rePtr <- compile flags re
    case rePtr of
        Left err -> pure (Left err)
        Right rp -> Right <$> find rp haystack 0

find :: RurePtr
     -> BS.ByteString -- ^ Unicode
     -> CSize -- ^ Start
     -> IO (Maybe RureMatch)
find rePtr haystack start' =
    allocaBytes {# sizeof rure_match #} $ \matchPtr -> do
        res <- BS.unsafeUseAsCStringLen haystack $ \(p, sz) ->
            rureFind rePtr (castPtr p) (fromIntegral sz) start' matchPtr
        if res
            then Just <$> rureMatchFromPtr matchPtr
            else pure Nothing

{-# NOINLINE hsSetMatches #-}
hsSetMatches :: RureFlags
             -> [BS.ByteString]
             -> BS.ByteString
             -> Either String [Bool]
hsSetMatches flags res haystack = unsafePerformIO $ do
    resPtr <- compileSet flags res
    case resPtr of
        Left err -> pure (Left err)
        Right rsp -> Right <$> setMatches rsp haystack 0

{-# NOINLINE hsSetIsMatch #-}
hsSetIsMatch :: RureFlags
             -> [BS.ByteString] -- ^ Needles (regex)
             -> BS.ByteString -- ^ Haystack
             -> Either String Bool
hsSetIsMatch flags res haystack = unsafePerformIO $ do
    resPtr <- compileSet flags res
    case resPtr of
        Left err -> pure (Left err)
        Right rsp -> Right <$> setIsMatch rsp haystack 0

{-# NOINLINE hsIsMatch #-}
hsIsMatch :: RureFlags
          -> BS.ByteString -- ^ Regex
          -> BS.ByteString -- ^ Haystack (unicode)
          -> Either String Bool
hsIsMatch flags re haystack = unsafePerformIO $ do
    rePtr <- compile flags re
    case rePtr of
        Left err -> pure (Left err)
        Right rp -> Right <$> isMatch rp haystack 0

setIsMatch :: RureSetPtr
           -> BS.ByteString -- ^ Unicode
           -> CSize -- ^ Start
           -> IO Bool
setIsMatch rsPtr haystack startϵ =
    BS.unsafeUseAsCStringLen haystack $ \(p, sz) ->
        rureSetIsMatch rsPtr (castPtr p) (fromIntegral sz) startϵ

setMatches :: RureSetPtr
           -> BS.ByteString
           -> CSize
           -> IO [Bool]
setMatches rsPtr haystack startϵ =
    BS.unsafeUseAsCStringLen haystack $ \(p, sz) -> do
        l <- fromIntegral <$> rureSetLen rsPtr
        allocaBytes l $ \boolPtr -> do
            rureSetMatches rsPtr (castPtr p) (fromIntegral sz) startϵ boolPtr
            fmap cBoolToBool <$> peekArray l boolPtr
    where cBoolToBool 0 = False
          cBoolToBool _ = True

isMatch :: RurePtr
        -> BS.ByteString -- ^ Unicode
        -> CSize -- ^ Start
        -> IO Bool
isMatch rePtr haystack start' =
    BS.unsafeUseAsCStringLen haystack $ \(p, sz) ->
        rureIsMatch rePtr (castPtr p) (fromIntegral sz) start'
