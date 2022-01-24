-- | See @rure.h@ for documentation + how to use.
module Regex.Rure.FFI ( -- * Types
                      -- ** Abstract types
                        Rure
                      , RureOptions
                      , RureError
                      , RureCaptures
                      , RureSet
                      , RureIter
                      , RureIterCaptureNames
                      -- ** Integer types
                      , UInt8
                      , UInt32
                      -- ** Types
                      , RureMatch (..)
                      , RureFlags
                      -- ** Pointer types (c2hs)
                      , RurePtr
                      , RureErrorPtr
                      , RureOptionsPtr
                      , RureIterPtr
                      , RureCapturesPtr
                      , RureSetPtr
                      , RureIterCaptureNamesPtr
                      -- * Functions
                      -- ** Allocation
                      , rureOptionsNew
                      , rureOptionsFree
                      , rureErrorNew
                      , rureErrorFree
                      , rureIterNew
                      , rureFree
                      , rureIterFree
                      , rureCapturesNew
                      , rureCapturesFree
                      , rureSetFree
                      , rureIterCaptureNamesNew
                      , rureIterCaptureNamesFree
                      -- ** Options
                      , rureOptionsSizeLimit
                      , rureOptionsDfaSizeLimit
                      , rureErrorMessage
                      -- ** Compilation
                      , rureCompile
                      , rureCompileMust
                      , rureCompileSet
                      -- ** Matching
                      , rureIsMatch
                      , rureFind
                      , rureIterNext
                      , rureIterNextCaptures
                      , rureCapturesAt
                      , rureCapturesLen
                      , rureFindCaptures
                      , rureShortestMatch
                      , rureCaptureNameIndex
                      , rureSetIsMatch
                      , rureSetMatches
                      , rureSetLen
                      , rureIterCaptureNamesNext
                      -- ** Flags
                      , rureFlagCaseI
                      , rureFlagMulti
                      , rureFlagDotNL
                      , rureFlagSwapGreed
                      , rureFlagSpace
                      , rureFlagUnicode
                      , rureDefaultFlags
                      -- ** String utilities
                      , rureEscapeMust
                      , rureCstringFree
                      ) where

import Data.Bits (Bits, (.|.), shift)
import Data.Coerce (coerce)
import Data.Int (Int32)
import Data.Semigroup (Semigroup (..))
import Foreign.C.String (CString)
import Foreign.C.Types (CBool, CSize)
import Foreign.Ptr (Ptr, castPtr)

#undef __arm64__
#define __arm__
#include <rure.h>

type UInt8 = {# type uint8_t #}
{#typedef uint8_t UInt8#}
{#default in `Ptr UInt8' [uint8_t *] id#} -- TODO: bytestring?

type UInt32 = {# type uint32_t #}

newtype RureFlags = RureFlags UInt32

instance Semigroup RureFlags where
    (<>) (RureFlags x) (RureFlags y) = RureFlags (x .|. y)

data Rure

data RureOptions

data RureMatch = RureMatch { start :: !CSize, end :: !CSize } deriving (Eq, Show)

data RureError

data RureIter

data RureCaptures

data RureIterCaptureNames

data RureSet

(<<) :: Bits a => a -> Int -> a
m << n = m `shift` n

rureFlagCaseI :: RureFlags
rureFlagCaseI = RureFlags ({# const RURE_FLAG_CASEI #})

rureFlagMulti :: RureFlags
rureFlagMulti = RureFlags ({# const RURE_FLAG_MULTI #})

rureFlagDotNL :: RureFlags
rureFlagDotNL = RureFlags ({# const RURE_FLAG_DOTNL #})

rureFlagSwapGreed :: RureFlags
rureFlagSwapGreed = RureFlags ({# const RURE_FLAG_SWAP_GREED #})

rureFlagSpace :: RureFlags
rureFlagSpace = RureFlags ({# const RURE_FLAG_SPACE #})

rureFlagUnicode :: RureFlags
rureFlagUnicode = RureFlags ({# const RURE_FLAG_UNICODE #})

rureDefaultFlags :: RureFlags
rureDefaultFlags = RureFlags ({# const RURE_FLAG_UNICODE #})

{# pointer *rure as RurePtr foreign finalizer rure_free as ^ -> Rure #}
{# pointer *rure_options as RureOptionsPtr foreign finalizer rure_options_free as ^ -> RureOptions #}
{# pointer *rure_error as RureErrorPtr foreign finalizer rure_error_free as ^ -> RureError #}
{# pointer *rure_iter as RureIterPtr foreign finalizer rure_iter_free as ^ -> RureIter #}
{# pointer *rure_captures as RureCapturesPtr foreign finalizer rure_captures_free as ^ -> RureCaptures #}
{# pointer *rure_set as RureSetPtr foreign finalizer rure_set_free as ^ -> RureSet #}
{# pointer *rure_iter_capture_names as RureIterCaptureNamesPtr foreign finalizer rure_iter_capture_names_free as ^ -> RureIterCaptureNames #}

{# fun unsafe rure_compile_must as ^ { `CString' } -> `Ptr Rure' id #}
{# fun unsafe rure_compile as ^ { `Ptr UInt8'
                         , coerce `CSize'
                         , coerce `RureFlags'
                         , `RureOptionsPtr'
                         , `RureErrorPtr'
                         } -> `Ptr Rure' id
  #}
{# fun unsafe rure_is_match as ^ { `RurePtr', `Ptr UInt8', coerce `CSize', coerce `CSize' } -> `Bool' #}
{# fun unsafe rure_find as ^ { `RurePtr'
                      , `Ptr UInt8'
                      , coerce `CSize'
                      , coerce `CSize'
                      , castPtr `Ptr RureMatch'
                      } -> `Bool'
  #}
{# fun unsafe rure_find_captures as ^ { `RurePtr'
                               , `Ptr UInt8'
                               , coerce `CSize'
                               , coerce `CSize'
                               , `RureCapturesPtr'
                               } -> `Bool'
  #}
{# fun unsafe rure_shortest_match as ^ { `RurePtr'
                                , `Ptr UInt8'
                                , coerce `CSize'
                                , coerce `CSize'
                                , castPtr `Ptr CSize'
                                } -> `Bool'
  #}
{# fun unsafe rure_capture_name_index as ^ { `RurePtr'
                                           , `CString'
                                           } -> `Int32'
  #}
{# fun unsafe rure_iter_capture_names_new as ^ { `RurePtr' } -> `Ptr RureIterCaptureNames' id #}
{# fun unsafe rure_iter_capture_names_next as ^ { `RureIterCaptureNamesPtr', id `Ptr CString' } -> `Bool' #}
{# fun unsafe rure_iter_new as ^ { `RurePtr' } -> `Ptr RureIter' id #}
{# fun unsafe rure_iter_next as ^ { `RureIterPtr'
                           , `Ptr UInt8'
                           , coerce `CSize'
                           , castPtr `Ptr RureMatch'
                           } -> `Bool'
  #}
{# fun unsafe rure_iter_next_captures as ^ { `RureIterPtr'
                                    , `Ptr UInt8'
                                    , coerce `CSize'
                                    , `RureCapturesPtr'
                                    } -> `Bool'
  #}
{# fun unsafe rure_captures_new as ^ { `RurePtr' } -> `Ptr RureCaptures' id #}
{# fun unsafe rure_captures_at as ^ { `RureCapturesPtr', coerce `CSize', castPtr `Ptr RureMatch' } -> `Bool' #}
{# fun unsafe rure_captures_len as ^ { `RureCapturesPtr' } -> `CSize' coerce #}
{# fun unsafe rure_options_new as ^ { } -> `Ptr RureOptions' id #}
{# fun unsafe rure_options_size_limit as ^ { `RureOptionsPtr', coerce `CSize' } -> `()' #}
{# fun unsafe rure_options_dfa_size_limit as ^ { `RureOptionsPtr', coerce `CSize' } -> `()' #}
{# fun unsafe rure_compile_set as ^ { id `Ptr (Ptr UInt8)'
                             , castPtr `Ptr CSize'
                             , coerce `CSize'
                             , coerce `RureFlags'
                             , `RureOptionsPtr'
                             , `RureErrorPtr'
                             } -> `Ptr RureSet' id
  #}
{# fun unsafe rure_set_is_match as ^ { `RureSetPtr'
                              , `Ptr UInt8'
                              , coerce `CSize'
                              , coerce `CSize'
                              } -> `Bool'
  #}
{# fun unsafe rure_set_matches as ^ { `RureSetPtr'
                             , `Ptr UInt8'
                             , coerce `CSize'
                             , coerce `CSize'
                             , castPtr `Ptr CBool'
                             } -> `Bool'
  #}
{# fun unsafe rure_set_len as ^ { `RureSetPtr' } -> `CSize' coerce #}
{# fun unsafe rure_error_new as ^ { } -> `Ptr RureError' id #}
{# fun unsafe rure_error_message as ^ { `RureErrorPtr' } -> `String' #}
{# fun unsafe rure_escape_must as ^ { `CString' } -> `CString' #}
{# fun unsafe rure_cstring_free as ^ { `CString' } -> `()' #}
