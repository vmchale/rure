-- | See @rure.h@ for documentation + how to use.
module Regex.Rure.FFI ( -- * Abstract types
                        Rure
                      , RureOptions
                      , RureError
                      , RureMatch (..)
                      -- * Types
                      , RurePtr
                      , RureErrorPtr
                      , RureOptionsPtr
                      , rureCompile
                      , rureOptionsNew
                      , rureErrorNew
                      , rureErrorMessage
                      , rureFree
                      , rureOptionsFree
                      , rureErrorFree
                      -- ** Matching
                      , rureIsMatch
                      , rureFind
                      -- ** Flags
                      , rureFlagCaseI
                      , rureFlagMulti
                      , rureFlagDotNL
                      , rureFlagSpace
                      , rureFlagUnicode
                      , rureDefaultFlags
                      ) where

import Data.Bits (Bits, (.|.), shift)
import Data.Coerce (coerce)
import Foreign.C.String (CString)
import Foreign.C.Types (CSize)
import Foreign.Ptr (Ptr, castPtr)

-- TODO: not this
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

data RureMatch = RureMatch !CSize !CSize deriving (Eq, Show)

data RureError

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

-- {# fun rure_compile_must as ^ { `CString' } -> `Ptr Rure' id #}
{# fun rure_compile as ^ { `Ptr UInt8', coerce `CSize', coerce `RureFlags', `RureOptionsPtr', `RureErrorPtr' } -> `Ptr Rure' id #}
{# fun rure_is_match as ^ { `RurePtr', `Ptr UInt8', coerce `CSize', coerce `CSize' } -> `Bool' #}
{# fun rure_find as ^ { `RurePtr', `Ptr UInt8', coerce `CSize', coerce `CSize', castPtr `Ptr RureMatch' } -> `Bool' #}
{# fun rure_options_new as ^ { } -> `Ptr RureOptions' id #}
{# fun rure_options_size_limit as ^ { `RureOptionsPtr', coerce `CSize' } -> `()' #}
{# fun rure_options_dfa_size_limit as ^ { `RureOptionsPtr', coerce `CSize' } -> `()' #}
{# fun rure_error_new as ^ { } -> `Ptr RureError' id #}
{# fun rure_error_message as ^ { `RureErrorPtr' } -> `String' #}
