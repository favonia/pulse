{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

module Sound.Pulse.Internal.PropList where

import Foreign.C
import Foreign.Safe
import Sound.Pulse.Internal.C2HS

#include <pulse/proplist.h>

data RawPropList
{#pointer *proplist as RawPropListPtr -> RawPropList #}

{#fun proplist_new as ^ {} -> `RawPropListPtr' id #}

{#fun proplist_free as ^ {id `RawPropListPtr' } -> `()' #}

{#fun proplist_sets as ^ {id `RawPropListPtr', `String', `String'} -> `Int' #}

{#fun proplist_gets as ^ {id `RawPropListPtr', `String'} -> `String' #}

{#fun proplist_unset as ^ {id `RawPropListPtr', `String'} -> `Int' #}

type PropListIterateState = Ptr ()
{#fun proplist_iterate as ^ {id `RawPropListPtr', id `Ptr PropListIterateState'} -> `String' #}

{#fun proplist_contains as ^ {id `RawPropListPtr', `String'} -> `Int' #}

{#fun proplist_clear as ^ {id `RawPropListPtr'} -> `()' #}

{#fun proplist_size as ^ {id `RawPropListPtr'} -> `Int' #}

{#fun proplist_isempty as ^ {id `RawPropListPtr'} -> `Bool' #}

{#fun proplist_equal as ^ {id `RawPropListPtr', id `RawPropListPtr'} -> `Int' #}
