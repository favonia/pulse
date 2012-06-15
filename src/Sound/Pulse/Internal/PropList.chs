{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

module Sound.Pulse.Internal.PropList where

import Foreign
import Foreign.C
import Sound.Pulse.Internal.C2HS

#include <pulse/proplist.h>

data RawPropList
{#pointer *proplist as RawPropListPtr -> RawPropList #}

{#fun proplist_new as ^ {} -> `RawPropListPtr' id #}

{#fun proplist_free as ^ {id `RawPropListPtr' } -> `()' #}

{#fun proplist_sets as ^ {id `RawPropListPtr', id `CString', id `CString'} -> `Int' #}

{#fun proplist_gets as ^ {id `RawPropListPtr', id `CString'} -> `CString' id #}

{#fun proplist_unset as ^ {id `RawPropListPtr', id `CString'} -> `Int' #}

{#fun proplist_contains as ^ {id `RawPropListPtr', id `CString'} -> `Int' #}

{#fun proplist_clear as ^ {id `RawPropListPtr'} -> `()' #}

{#fun proplist_size as ^ {id `RawPropListPtr'} -> `Int' #}

{#fun proplist_isempty as ^ {id `RawPropListPtr'} -> `Int' #}

{#fun proplist_equal as ^ {id `RawPropListPtr', id `RawPropListPtr'} -> `Int' #}

type PPtr = Ptr (Ptr ())
{#fun proplist_iterate as ^ {id `RawPropListPtr', id `PPtr'} -> `CString' id #}
