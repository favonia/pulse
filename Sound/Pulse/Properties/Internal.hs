{-# LANGUAGE TemplateHaskell #-}
{- |
Module      :  Sound.Pulse.Properties.Internal
Copyright   :  (c) Favonia
License     :  BSD3

Maintainer  :  favonia@gmail.com
Stability   :  experimental
Portability :  non-portable (GHC only)

This module provides Template Haskell generators for 'PropTag'.
-}
module Sound.Pulse.Properties.Internal where

import Language.Haskell.TH

data PropSpec = PropSpec
    { propName :: String
    , propHaskellName :: String
    , propArgType :: Name
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
propSpecs :: [PropSpec]
propSpecs =
    [ PropSpec "media.name"      "MediaName"       ''String  undefined undefined
    , PropSpec "media.title"     "MediaTitle"      ''String  undefined undefined
    , PropSpec "media.artist"    "MediaArtist"     ''String  undefined undefined
    , PropSpec "media.copyright" "MediaCopyright"  ''String  undefined undefined
    , PropSpec "media.software"  "MediaSoftware"   ''String  undefined undefined
    , PropSpec "media.language"  "MediaLanguage"   ''String  undefined undefined
    , PropSpec "media.filename"  "MediaFilename"   ''String  undefined undefined
    , PropSpec "media.icon_name" "MediaIconName"   ''String  undefined undefined
    , PropSpec "media.role"      "MediaRole"       ''Role    undefined undefined
    ]

-- |Generate 'PropTag'
genPropTag :: Q [Dec]
genPropTag = do
    param <- newName "a"
    -- tagName' <- newName "PropTag"
    return $
        [ DataD
            []
            (mkName "PropTag")
            [PlainTV param]
            [ForallC
                []
                [EqualP (VarT param) (ConT $ propArgType pQ)]
                (NormalC (mkName $ propHaskellName pQ) []) | pQ <- propSpecs ]
            []
        ]
