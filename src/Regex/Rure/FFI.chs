module Regex.Rure.FFI ( -- * Abstract types
                        Rure
                      , RureOptions
                      ) where

import Foreign.C.String (CString)
import Foreign.C.Types (CSize)
import Foreign.Ptr (Ptr)

-- TODO: not this
#undef __arm64__
#define __arm__
#include <rure.h>

data Rure

data RureOptions

data RureMatch = RureMatch !CSize !CSize

data RureCaptures

data RureIter

data RureIterCaptureNames

data RureError

{# pointer *rure as RurePtr foreign finalizer rure_free as ^ -> Rure #}

{# fun pure rure_compile_must as ^ { `CString' } -> `Ptr Rure' id #}
