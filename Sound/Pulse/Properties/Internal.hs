module Sound.Pulse.Properties.Internal where

import Language.Haskell.TH
--import Text.Parsec

data PropQ = PropQ
    { propName :: String
    , propHaskellName :: String
    , propArgType :: String
    , propReader :: String
    , propPrinter :: String
    }

-- |Access mode. Used in 'DeviceAccessMode'.
data AccessMode = Mmap | MmapRewrite | Serial

-- |Bus type. Used in 'DeviceBus'.
data Bus = Isa | Pci | Usb | Firewire | Bluetooth

-- |Class of a device. Used in 'DeviceClass'.
data Class = Sound | Modem | Monitor | Filter

-- |Form factor. Used in 'DeviceFormFactor'.
data FormFactor = Internal | Speaker | Handset | Tv | Webcam | Microphone | Headset | Headphone | HandsFree | Car | Hifi | Computer | Portable

-- |Button clicked in an event. Used in 'EventMouseButton'.
data MouseButton = MouseLeft | MouseMiddle | MouseRight

-- |Role of this media. Used in 'MediaRole' and 'DeviceIntendedRoles'.
data Role = Video | Music | Game | Event | Phone | Animation | Production | A11y | Test

-- |Metadata for Template Haskell
propQs :: [PropQ]
propQs =
    [ PropQ "media.name"      "MediaName"       "String"  undefined undefined
    , PropQ "media.title"     "MediaTitle"      "String"  undefined undefined
    , PropQ "media.artist"    "MediaArtist"     "String"  undefined undefined
    , PropQ "media.copyright" "MediaCopyright"  "String"  undefined undefined
    , PropQ "media.software"  "MediaSoftware"   "String"  undefined undefined
    , PropQ "media.language"  "MediaLanguage"   "String"  undefined undefined
    , PropQ "media.filename"  "MediaFilename"   "String"  undefined undefined
    , PropQ "media.icon_name" "MediaIconName"   "String"  undefined undefined
    , PropQ "media.role"      "MediaRole"       "Role"    undefined undefined
    ]

-- |Generate 'PropTag'
genPropTag :: [PropQ] -> Q [Dec]
genPropTag propQs = do
    param <- newName "a"
    tagName' <- newName "PropTag"
    return $
      [DataD
        []
        tagName'
        [PlainTV param]
        [ForallC
            []
            [EqualP (VarT param) (ConT $ mkName $ propArgType pQ)]
            (NormalC (mkName $ propHaskellName pQ) []) | pQ <- propQs ]
        []]

