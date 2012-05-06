{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

module Sound.Pulse.Internal.PropList where

import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Sound.Pulse.Internal.C2HS

#include <pulse/proplist.h>

data RawPropList
{#pointer *pa_proplist as RawPropListPtr -> RawPropList #}

{#fun pa_proplist_new as ^ {} -> `RawPropListPtr' id #}

{#fun pa_proplist_free as ^ {id `RawPropListPtr' } -> `()' #}

{#fun pa_proplist_sets as ^ {id `RawPropListPtr', id `Ptr CChar', id `Ptr CChar'} -> `Int' #}

{#fun pa_proplist_gets as ^ {id `RawPropListPtr', id `Ptr CChar'} -> `Ptr CChar' id #}

{#fun pa_proplist_unset as ^ {id `RawPropListPtr', id `Ptr CChar'} -> `Int' #}

{#fun pa_proplist_contains as ^ {id `RawPropListPtr', id `Ptr CChar'} -> `Int' #}

{#fun pa_proplist_clear as ^ {id `RawPropListPtr'} -> `()' #}

{#fun pa_proplist_size as ^ {id `RawPropListPtr'} -> `Int' #}

{#fun pa_proplist_isempty as ^ {id `RawPropListPtr'} -> `Int' #}

{#fun pa_proplist_equal as ^ {id `RawPropListPtr', id `RawPropListPtr'} -> `Int' #}

