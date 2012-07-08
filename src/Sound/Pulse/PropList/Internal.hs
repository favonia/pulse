{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{- |
Module      :  Sound.Pulse.PropList.Internal
Copyright   :  (c) Favonia
License     :  BSD3

Maintainer  :  favonia@gmail.com
Stability   :  experimental
Portability :  non-portable (GHC only)

This module provides Template Haskell generators for 'PropTag'.
-}
module Sound.Pulse.PropList.Internal where

import Data.List (intercalate)
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString, toString)
#if !defined(mingw32_HOST_OS)
import System.Posix.Types (ProcessID)
#endif

import Data.Dependent.Sum
import Data.GADT.Compare
import Data.GADT.Show

import Language.Haskell.TH
import Data.Typeable (Typeable)

-- | Specification of a property.
data PropSpec = TextPropSpec { propRawName :: String
                             , propHaskellName :: String
                             , propValueType :: Name
                             , propToRawValue :: Name
                             , propFromRawValue :: Name
                             }
              | BinaryPropSpec { propRawName :: String
                               , propHaskellName :: String
                               , propValueType :: Name
                               , propToRawValue :: Name
                               , propFromRawValue :: Name
                               }

-- | Access mode. Used in 'DeviceAccessMode'.
data AccessMode = Mmap | MmapRewrite | Serial deriving (Eq, Show, Ord)

-- | Out marshaller for 'AccessMode'.
toRawAccessMode :: AccessMode -> String
toRawAccessMode mode = case mode of Mmap        -> "mmap"
                                    MmapRewrite -> "mmap_rewrite"
                                    Serial      -> "serial"

-- | In marshaller for 'AccessMode'.
fromRawAccessMode :: String -> AccessMode
fromRawAccessMode modeStr = case modeStr of "mmap"         -> Mmap
                                            "mmap_rewrite" -> MmapRewrite
                                            "serial"       -> Serial
                                            _ -> error "unknown access mode"

-- | Bus type. Used in 'DeviceBus'.
data Bus = Isa | Pci | Usb | Firewire | Bluetooth deriving (Eq, Show, Ord)

-- | Out marshaller for 'Bus'.
toRawBus :: Bus -> String
toRawBus bus = case bus of Isa       -> "isa"
                           Pci       -> "pci"
                           Usb       -> "usb"
                           Firewire  -> "firewire"
                           Bluetooth -> "bluetooth"

-- | In marshaller for 'Bus'.
fromRawBus :: String -> Bus
fromRawBus busStr = case busStr of "isa"       -> Isa
                                   "pci"       -> Pci
                                   "usb"       -> Usb
                                   "firewire"  -> Firewire
                                   "bluetooth" -> Bluetooth
                                   _ -> error "unknown bus"

-- | Class of a device. Used in 'DeviceClass'.
data Class = Sound | Modem | Monitor | Filter deriving (Eq, Show, Ord)

-- | Out marshaller for 'Class'.
toRawClass :: Class -> String
toRawClass cls = case cls of Sound   -> "sound"
                             Modem   -> "modem"
                             Monitor -> "monitor"
                             Filter  -> "filter"

-- | In marshaller for 'Class'.
fromRawClass :: String -> Class
fromRawClass clsStr = case clsStr of "sound"   -> Sound
                                     "modem"   -> Modem
                                     "monitor" -> Monitor
                                     "filter"  -> Filter
                                     _ -> error "unknown device class"


-- | Form factor. Used in 'DeviceFormFactor'.
data FormFactor = Internal | Speaker | Handset | Tv | Webcam
                | Microphone | Headset | Headphone | HandsFree
                | Car | Hifi | Computer | Portable
    deriving (Eq, Show, Ord)

-- | Out marshaller for 'FormFactor'.
toRawFormFactor :: FormFactor -> String
toRawFormFactor formF = case formF of Internal   -> "internal"
                                      Speaker    -> "speaker"
                                      Handset    -> "handset"
                                      Tv         -> "tv"
                                      Webcam     -> "webcam"
                                      Microphone -> "microphone"
                                      Headset    -> "headset"
                                      Headphone  -> "headphone"
                                      HandsFree  -> "hands-free"
                                      Car        -> "car"
                                      Hifi       -> "hifi"
                                      Computer   -> "computer"
                                      Portable   -> "portable"

-- | In marshaller for 'FormFactor'.
fromRawFormFactor :: String -> FormFactor
fromRawFormFactor formFStr = case formFStr of "internal"   -> Internal
                                              "speaker"    -> Speaker
                                              "handset"    -> Handset
                                              "tv"         -> Tv
                                              "webcam"     -> Webcam
                                              "microphone" -> Microphone
                                              "headset"    -> Headset
                                              "headphone"  -> Headphone
                                              "hands-free" -> HandsFree
                                              "car"        -> Car
                                              "hifi"       -> Hifi
                                              "computer"   -> Computer
                                              "portable"   -> Portable
                                              _ -> error "unknown form factor"

-- | Button clicked in an event. Used in 'EventMouseButton'.
data MouseButton = MouseLeft | MouseMiddle | MouseRight deriving (Eq, Show, Ord)

-- | Out marshaller for 'MouseButton'.
toRawMouseButton :: MouseButton -> String
toRawMouseButton btn = case btn of MouseLeft   -> "0"
                                   MouseMiddle -> "1"
                                   MouseRight  -> "2"

-- | In marshaller for 'MouseButton'.
fromRawMouseButton :: String -> MouseButton
fromRawMouseButton btnNoStr = case btnNoStr of "0" -> MouseLeft
                                               "1" -> MouseMiddle
                                               "2" -> MouseRight
                                               _ -> error "unknown mouse button"

-- | Role of this media. Used in 'MediaRole' and 'DeviceIntendedRoles'.
data Role = Video | Music | Game | Event | Phone | Animation | Production | A11y | Test
    deriving (Eq, Show, Ord)

-- | Out marshaller for 'Role'.
toRawRole :: Role -> String
toRawRole r = case r of Video -> "video"
                        Music -> "music"
                        Game  -> "game"
                        Event -> "event"
                        Phone -> "phone"
                        Animation -> "animation"
                        Production -> "production"
                        A11y -> "a11y"
                        Test -> "test"

-- | In marshaller for 'Role'.
fromRawRole :: String -> Role
fromRawRole roleStr = case roleStr of "video" -> Video
                                      "music" -> Music
                                      "game" -> Game
                                      "event" -> Event
                                      "phone" -> Phone
                                      "animation" -> Animation
                                      "production" -> Production
                                      "a11y" -> A11y
                                      "test" -> Test
                                      _ -> error "unknown role"

-- | List of Roles. This type alias is created purely for our usage of Template Haskell.
--   Used in 'WindowDesktop'.
type IntendedRoles = [Role]

-- | Out marshaller for 'IntendedRole'.
toRawIntendedRoles :: IntendedRoles -> String
toRawIntendedRoles = intercalate "," . map toRawRole

-- | In marshaller for 'IntendedRole'.
fromRawIntendedRoles :: String -> IntendedRoles
fromRawIntendedRoles [] = []
fromRawIntendedRoles s =
  let (first, rest) = break (== ',') s
  in fromRawRole first : case rest of
      [] -> []
      (_:rest') -> fromRawIntendedRoles rest'

-- | List of indexes. This type alias is created purely for our usage of Template Haskell.
--   Used in 'WindowDesktop'.
type Desktop = [Int]

-- | Out marshaller for 'Desktop'.
toRawDesktop :: Desktop -> String
toRawDesktop = intercalate "," . map show

-- | In marshaller for 'Desktop'.
fromRawDesktop :: String -> Desktop
fromRawDesktop [] = []
fromRawDesktop s =
    let (first, rest) = break (== ',') s
    in read first : case rest of
        [] -> []
        (_:rest') -> fromRawDesktop rest'

-- | Metadata for Template Haskell.
propSpecs :: [PropSpec]
propSpecs =
    [ TextPropSpec "media.name"         "MediaName"         ''String      'id  'id
    , TextPropSpec "media.title"        "MediaTitle"        ''String      'id  'id
    , TextPropSpec "media.artist"       "MediaArtist"       ''String      'id  'id
    , TextPropSpec "media.copyright"    "MediaCopyright"    ''String      'id  'id
    , TextPropSpec "media.software"     "MediaSoftware"     ''String      'id  'id
    , TextPropSpec "media.language"     "MediaLanguage"     ''String      'id  'id
    , TextPropSpec "media.filename"     "MediaFilename"     ''String      'id  'id
    , BinaryPropSpec "media.icon"       "MediaIcon"         ''ByteString  'id  'id
    , TextPropSpec "media.icon_name"    "MediaIconName"     ''String      'id  'id
    , TextPropSpec "media.role"         "MediaRole"         ''Role        'toRawRole  'fromRawRole

    , TextPropSpec "filter.want"        "FilterWant"        ''String      'id  'id
    , TextPropSpec "filter.apply"       "FilterApply"       ''String      'id  'id
    , TextPropSpec "filter.suppress"    "FilterSuppress"    ''String      'id  'id

    , TextPropSpec "event.id"           "EventId"           ''String      'id  'id
    , TextPropSpec "event.description"  "EventDescription"  ''String      'id  'id
    , TextPropSpec "event.mouse.x"      "EventMouseX"       ''Int         'show  'read
    , TextPropSpec "event.mouse.y"      "EventMouseY"       ''Int         'show  'read
    , TextPropSpec "event.mouse.hpos"   "EventMouseHpos"    ''Double      'show  'read
    , TextPropSpec "event.mouse.vpos"   "EventMouseVpos"    ''Double      'show  'read
    , TextPropSpec "event.mouse.button" "EventMouseButton"  ''MouseButton 'toRawMouseButton  'fromRawMouseButton

    , TextPropSpec "window.name"        "WindowName"        ''String      'id  'id
    , TextPropSpec "window.id"          "WindowId"          ''String      'id  'id
    , BinaryPropSpec "window.icon"      "WindowIcon"        ''ByteString  'id  'id
    , TextPropSpec "window.icon_name"   "WindowIconName"    ''String      'id  'id
    , TextPropSpec "window.x"           "WindowX"           ''Int         'show  'read
    , TextPropSpec "window.y"           "WindowY"           ''Int         'show  'read
    , TextPropSpec "window.width"       "WindowWidth"       ''Int         'show  'read
    , TextPropSpec "window.height"      "WindowHeight"      ''Int         'show  'read
    , TextPropSpec "window.hpos"        "WindowHpos"        ''Double      'show  'read
    , TextPropSpec "window.vpos"        "WindowVpos"        ''Double      'show  'read
    , TextPropSpec "window.desktop"     "WindowDesktop"     ''Desktop     'toRawDesktop  'fromRawDesktop
    , TextPropSpec "window.x11.display" "WindowX11Display"  ''String      'id  'id
    , TextPropSpec "window.x11.screen"  "WindowX11Screen"   ''Int         'show  'read
    , TextPropSpec "window.x11.monitor" "WindowX11Monitor"  ''Int         'show  'read
    , TextPropSpec "window.x11.xid"     "WindowX11Xid"      ''Int         'show  'read

    , TextPropSpec "application.name"           "ApplicationName"          ''String        'id  'id
    , TextPropSpec "application.id"             "ApplicationId"            ''String        'id  'id
    , TextPropSpec "application.version"        "ApplicationVersion"       ''String        'id  'id
    , BinaryPropSpec "application.icon"         "ApplicationIcon"          ''ByteString    'id  'id
    , TextPropSpec "application.icon_name"      "ApplicationIconName"      ''String        'id  'id
    , TextPropSpec "application.language"       "ApplicationLanguage"      ''String        'id  'id
#if !defined(mingw32_HOST_OS)
    , TextPropSpec "application.process.id"     "ApplicationProcessId"     ''ProcessID     'show  'read
#endif
    , TextPropSpec "application.process.binary" "ApplicationProcessBinary" ''String        'id  'id
    , TextPropSpec "application.process.user"   "ApplicationProcessUser"   ''String        'id  'id
    , TextPropSpec "application.process.host"   "ApplicationProcessHost"   ''String        'id  'id
    , TextPropSpec "application.process.machine_id" "ApplicationProcessMachineId" ''String 'id  'id
    , TextPropSpec "application.process.session_id" "ApplicationProcessSessionId" ''Int    'show  'read

    , TextPropSpec "device.string"              "DeviceString"             ''String        'id  'id
    , TextPropSpec "device.api"                 "DeviceApi"                ''String        'id  'id
    , TextPropSpec "device.description"         "DeviceDescription"        ''String        'id  'id
    , TextPropSpec "device.bus_path"            "DeviceBusPath"            ''String        'id  'id
    , TextPropSpec "device.serial"              "DeviceSerial"             ''String        'id  'id
    , TextPropSpec "device.vendor.id"           "DeviceVendorId"           ''String        'id  'id
    , TextPropSpec "device.vendor.name"         "DeviceVendorName"         ''String        'id  'id
    , TextPropSpec "device.product.id"          "DeviceProductId"          ''String        'id  'id
    , TextPropSpec "device.product.name"        "DeviceProductName"        ''String        'id  'id
    , TextPropSpec "device.class"               "DeviceClass"              ''Class         'toRawClass       'fromRawClass
    , TextPropSpec "device.form_factor"         "DeviceFormFactor"         ''FormFactor    'toRawFormFactor  'fromRawFormFactor
    , TextPropSpec "device.bus"                 "DeviceBus"                ''Bus           'toRawBus         'fromRawBus
    , BinaryPropSpec "device.icon"              "DeviceIcon"               ''ByteString    'id  'id
    , TextPropSpec "device.icon_name"           "DeviceIconName"           ''String        'id  'id
    , TextPropSpec "device.access_mode"         "DeviceAccessMode"         ''AccessMode    'toRawAccessMode  'fromRawAccessMode
    , TextPropSpec "device.master_device"       "DeviceMasterDevice"       ''String        'id  'id
    , TextPropSpec "device.buffering.buffer_size"   "DeviceBufferingBufferSize"   ''Int    'show  'read
    , TextPropSpec "device.buffering.fragment_size" "DeviceBufferingFragmentSize" ''Int    'show  'read
    , TextPropSpec "device.profile.name"        "DeviceProfileName"        ''String        'id  'id
    , TextPropSpec "device.intended_roles"      "DeviceIntendedRoles"      ''IntendedRoles 'toRawIntendedRoles  'fromRawIntendedRoles
    , TextPropSpec "device.profile.description" "DeviceProfileDescription" ''String        'id  'id
    ]

-- | Generate 'PropTag'.
genPropTag :: Q [Dec]
genPropTag =
    let param = mkName "a" in
    return [DataD [] (mkName "PropTag")
        [PlainTV param]
        [ ForallC []
            [EqualP (VarT param) (ConT $ propValueType ps)]
            (NormalC (mkName $ propHaskellName ps) [])
        | ps <- propSpecs
        ] [''Typeable]]

-- | Generate the instance for 'GEq'.
deriveGEqPropTag :: Q [Dec]
deriveGEqPropTag =
    return [InstanceD []
        (ConT ''GEq `AppT` ConT (mkName "PropTag"))
        [FunD 'geq $
            [ Clause [pat, pat] (NormalB (ConE 'Just `AppE` ConE 'Refl)) []
            | ps <- propSpecs
            , let pat = ConP (mkName $ propHaskellName ps) []
            ]
            ++
            [ Clause [WildP, WildP] (NormalB $ ConE 'Nothing) []
            ]]]

-- | Generate the instance for 'EqTag'.
deriveEqTagPropTag :: Q [Dec]
deriveEqTagPropTag = do
    let propTag = ConT $ mkName "PropTag"
    return [InstanceD []
        (ConT ''EqTag `AppT` propTag)
        [FunD 'eqTagged $
            [ Clause [pat, pat] (NormalB $ VarE '(==)) []
            | ps <- propSpecs
            , let pat = ConP (mkName $ propHaskellName ps) []
            ]
            ++
            [Clause
                [WildP, WildP]
                (NormalB $ VarE 'error `AppE` LitE (StringL "incomparable"))
                []]]]

-- | Generate the instance for 'GCompare'.
deriveGComparePropTag :: Q [Dec]
deriveGComparePropTag =
    return [InstanceD []
        (ConT ''GCompare `AppT` ConT (mkName "PropTag"))
        [FunD 'gcompare $ concat
            [
                [ Clause [pat, pat] (NormalB (ConE 'GEQ)) []
                , Clause [WildP, pat] (NormalB (ConE 'GLT)) []
                , Clause [pat, WildP] (NormalB (ConE 'GGT)) []
                ]
            | ps <- propSpecs
            , let pat = ConP (mkName $ propHaskellName ps) []
            ]]]

-- | Generate the instance for 'OrdTag'.
deriveOrdTagPropTag :: Q [Dec]
deriveOrdTagPropTag = do
    let propTag = ConT $ mkName "PropTag"
    return [InstanceD []
        (ConT ''OrdTag `AppT` propTag)
        [FunD 'compareTagged $
            [ Clause [pat, pat] (NormalB $ VarE 'compare) []
            | ps <- propSpecs
            , let pat = ConP (mkName $ propHaskellName ps) []
            ]
            ++
            [Clause
                [WildP, WildP]
                (NormalB $ VarE 'error `AppE` LitE (StringL "incomparable"))
                []]]]

-- | Generate the instance for 'GShow'.
deriveGShowPropTag :: Q [Dec]
deriveGShowPropTag = do
    let propTag = ConT $ mkName "PropTag"
    return [InstanceD []
        (ConT ''GShow `AppT` propTag)
        [FunD 'gshowsPrec
            [ Clause [WildP, pat] (NormalB $ VarE '(++) `AppE` haskellName) []
            | ps <- propSpecs
            , let pat = ConP (mkName $ propHaskellName ps) []
            , let haskellName = LitE $ StringL $ propHaskellName ps
            ]]]

-- | Generate the instance for 'ShowTag'.
deriveShowTagPropTag :: Q [Dec]
deriveShowTagPropTag = do
    let propTag = ConT $ mkName "PropTag"
    return [InstanceD []
        (ConT ''ShowTag `AppT` propTag)
        [FunD 'showTaggedPrec
            [ Clause [pat] (NormalB $ VarE 'showsPrec) []
            | ps <- propSpecs
            , let pat = ConP (mkName $ propHaskellName ps) []
            ]]]

isTextProp :: PropSpec -> Bool
isTextProp (TextPropSpec {}) = True
isTextProp (BinaryPropSpec {}) = False

-- | Generate the in marshaller for 'PropList'.
genToKeyValue :: Q [Dec]
genToKeyValue = do
    let propTag = ConT $ mkName "PropTag"
    let dsumPropTag = ConT ''DSum `AppT` propTag
    let keyvalue = TupleT 2 `AppT` ConT ''String `AppT` ConT ''ByteString
    let func = mkName "toKeyValue"
    let var = mkName "x"
    return
        [ SigD func $ ArrowT `AppT` dsumPropTag `AppT` keyvalue
        , FunD func
            [ Clause
                [InfixP pat '(:=>) (VarP var)]
                (NormalB $ TupE
                    [ LitE $ StringL $ propRawName ps
                    , if isTextProp ps
                        then VarE 'fromString `AppE` serialized
                        else serialized
                    ]) []
            | ps <- propSpecs
            , let pat = ConP (mkName $ propHaskellName ps) []
            , let serialized = VarE (propToRawValue ps) `AppE` VarE var
            ]]

-- | Generate the out marshaller for 'PropList'.
genFromKeyValue :: Q [Dec]
genFromKeyValue = do
    let propTag = ConT $ mkName "PropTag"
    let dsumPropTag = ConT ''DSum `AppT` propTag
    let stringArrow = (ArrowT `AppT` ConT ''String `AppT`)
    let byteStringArrow = (ArrowT `AppT` ConT ''ByteString `AppT`)
    let func = mkName "fromKeyValue"
    let var = mkName "x"
    return
        [ SigD func $ stringArrow $ byteStringArrow dsumPropTag
        , FunD func $
            [ Clause
                [ LitP $ StringL $ propRawName ps
                , VarP var
                ]
                (NormalB $
                    ConE '(:=>)
                    `AppE`
                    ConE (mkName $ propHaskellName ps)
                    `AppE`
                    (VarE (propFromRawValue ps) `AppE`
                        if isTextProp ps
                            then VarE 'toString `AppE` VarE var
                            else VarE var))
                []
            | ps <- propSpecs
            ]
            ++
            -- XXX: poor error handling
            [Clause
                [WildP, WildP]
                (NormalB $ VarE 'error `AppE` LitE (StringL "unknown property name"))
                []]]
