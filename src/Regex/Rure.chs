module Regex.Rure ( -- * Higher-level functions
                    hsMatches
                  , hsIsMatch
                  , hsSetIsMatch
                  , hsFind
                  , hsSetMatches
                  -- * Functions in 'IO'.
                  , compile
                  , compileSet
                  , isMatch
                  , setIsMatch
                  , setMatches
                  , find
                  , matches
                  , matches'
                  , mkIter
                  , findCaptures
                  , captures
                  -- * Types
                  , RureMatch (..)
                  -- ** Pointer types
                  , RurePtr
                  , RureIterPtr
                  , RureSetPtr
                  -- * Options/flags
                  , RureFlags (..)
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

#include <rure.h>

infixr 9 ?
infixr 9 ??

x ? b = if b then x else pure Nothing
x ?? b = (Just<$>x) ? b

capturesAt :: RureCapturesPtr -> CSize -> IO (Maybe RureMatch)
capturesAt rcp sz =
    allocaBytes {# sizeof rure_match #} $ \matchPtr -> do
    res <- rureCapturesAt rcp sz matchPtr
    rureMatchFromPtr matchPtr ?? res

{-# DEPRECATED mkIter "This creates a stateful pointer in an otherwise pure API" #-}
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

-- | @since 0.1.2.0
matches' :: RurePtr -> BS.ByteString -> IO [RureMatch]
matches' rp haystack = do
    ri <- mkIter rp
    matches ri haystack

{-# DEPRECATED matches "Use matches', which is not stateful" #-}
matches :: RureIterPtr
        -> BS.ByteString
        -> IO [RureMatch]
matches reIPtr haystack = do
    res <- next
    case res of
        Nothing -> pure []
        Just m -> (m:) <$> matches reIPtr haystack
  where
    next :: IO (Maybe RureMatch)
    next = allocaBytes {# sizeof rure_match #} $ \matchPtr -> do
        res <- BS.unsafeUseAsCStringLen haystack $ \(p, sz) ->
            rureIterNext reIPtr (castPtr p) (fromIntegral sz) matchPtr
        rureMatchFromPtr matchPtr ?? res

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

allocCapPtr :: RurePtr -> IO RureCapturesPtr
allocCapPtr rp = do
    capPtr <- rureCapturesNew rp
    castForeignPtr <$> newForeignPtr rureCapturesFree (castPtr capPtr)

-- | @since 0.1.2.0
captures :: RurePtr
         -> BS.ByteString
         -> CSize -- ^ Index (for captures)
         -> IO [RureMatch]
captures re haystack n = do
    capPtr <- allocCapPtr re
    reIPtr <- mkIter re
    loop capPtr reIPtr
  where
    loop :: RureCapturesPtr -- ^ For results
         -> RureIterPtr
         -> IO [RureMatch]
    loop capPtr reIPtr = do
        res <- next n
        case res of
            Nothing -> pure []
            Just m -> (m:) <$> loop capPtr reIPtr
      where
        next :: CSize -- ^ Index (captures)
             -> IO (Maybe RureMatch)
        next ix = do
            res <- BS.unsafeUseAsCStringLen haystack $ \(p, sz) ->
                rureIterNextCaptures reIPtr (castPtr p) (fromIntegral sz) capPtr
            capturesAt capPtr ix ? res

-- | @since 0.1.2.0
findCaptures :: RurePtr
             -> BS.ByteString
             -> CSize -- ^ Index (captures)
             -> CSize -- ^ Start
             -> IO (Maybe RureMatch)
findCaptures rp haystack ix start' = do
    capFp <- allocCapPtr rp
    res <- BS.unsafeUseAsCStringLen haystack $ \(p, sz) ->
        rureFindCaptures rp (castPtr p) (fromIntegral sz) start' capFp
    capturesAt capFp ix ? res

find :: RurePtr
     -> BS.ByteString -- ^ Unicode
     -> CSize -- ^ Start
     -> IO (Maybe RureMatch)
find rePtr haystack start' =
    allocaBytes {# sizeof rure_match #} $ \matchPtr -> do
        res <- BS.unsafeUseAsCStringLen haystack $ \(p, sz) ->
            rureFind rePtr (castPtr p) (fromIntegral sz) start' matchPtr
        rureMatchFromPtr matchPtr ?? res

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

setMatches :: RureSetPtr -> BS.ByteString -> CSize -> IO [Bool]
setMatches rsPtr haystack startϵ =
    BS.unsafeUseAsCStringLen haystack $ \(p, sz) -> do
        l <- fromIntegral <$> rureSetLen rsPtr
        allocaBytes l $ \boolPtr -> do
            rureSetMatches rsPtr (castPtr p) (fromIntegral sz) startϵ boolPtr
            map cB <$> peekArray l boolPtr
    where cB 0 = False
          cB _ = True

isMatch :: RurePtr
        -> BS.ByteString -- ^ Unicode
        -> CSize -- ^ Start
        -> IO Bool
isMatch rePtr haystack start' =
    BS.unsafeUseAsCStringLen haystack $ \(p, sz) ->
        rureIsMatch rePtr (castPtr p) (fromIntegral sz) start'
