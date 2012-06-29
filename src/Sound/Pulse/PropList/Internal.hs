{-# LANGUAGE TemplateHaskell #-}
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

import qualified Data.ByteString as B
import Language.Haskell.TH
import Data.Dependent.Sum
import Data.GADT.Compare

data PropSpec = PropSpec
    { propRawName :: String
    , propHaskellName :: String
    , propValueType :: Name
    , propToRawValue :: Name
    , propFromRawValue :: Name
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

-- |List of strings. This type alias is created purely for our usage of Template Haskell.
type StringList = [String]

-- |Metadata for Template Haskell
propSpecs :: [PropSpec]
propSpecs =
    [ PropSpec "media.name"         "MediaName"         ''String      'undefined  'undefined
    , PropSpec "media.title"        "MediaTitle"        ''String      'undefined  'undefined
    , PropSpec "media.artist"       "MediaArtist"       ''String      'undefined  'undefined
    , PropSpec "media.copyright"    "MediaCopyright"    ''String      'undefined  'undefined
    , PropSpec "media.software"     "MediaSoftware"     ''String      'undefined  'undefined
    , PropSpec "media.language"     "MediaLanguage"     ''String      'undefined  'undefined
    , PropSpec "media.filename"     "MediaFilename"     ''String      'undefined  'undefined
    , PropSpec "media.icon_name"    "MediaIconName"     ''String      'undefined  'undefined
    , PropSpec "media.role"         "MediaRole"         ''Role        'undefined  'undefined
    , PropSpec "filter.want"        "FilterWant"        ''String      'undefined  'undefined
    , PropSpec "filter.apply"       "FilterApply"       ''String      'undefined  'undefined
    , PropSpec "filter.suppress"    "FilterSuppress"    ''String      'undefined  'undefined
    , PropSpec "event.id"           "EventId"           ''String      'undefined  'undefined
    , PropSpec "event.description"  "EventDescription"  ''String      'undefined  'undefined
    , PropSpec "event.mouse.x"      "EventMouseX"       ''Int         'undefined  'undefined
    , PropSpec "event.mouse.y"      "EventMouseY"       ''Int         'undefined  'undefined
    , PropSpec "event.mouse.hpos"   "EventMouseHpos"    ''Double      'undefined  'undefined
    , PropSpec "event.mouse.vpos"   "EventMouseVpos"    ''Double      'undefined  'undefined
    , PropSpec "event.mouse.button" "EventMouseButton"  ''MouseButton 'undefined  'undefined
    , PropSpec "window.name"        "WindowName"        ''String      'undefined  'undefined
    , PropSpec "window.id"          "WindowId"          ''String      'undefined  'undefined
    , PropSpec "window.icon_name"   "WindowIconName"    ''String      'undefined  'undefined
    , PropSpec "window.x"           "WindowX"           ''Int         'undefined  'undefined
    , PropSpec "window.y"           "WindowY"           ''Int         'undefined  'undefined
    , PropSpec "window.width"       "WindowWidth"       ''Int         'undefined  'undefined
    , PropSpec "window.height"      "WindowHeight"      ''Int         'undefined  'undefined
    , PropSpec "window.hpos"        "WindowHpos"        ''Double      'undefined  'undefined
    , PropSpec "window.vpos"        "WindowVpos"        ''Double      'undefined  'undefined
    , PropSpec "window.desktop"     "WindowDesktop"     ''StringList  'undefined  'undefined
    , PropSpec "window.x11.display" "WindowX11Display"  ''String      'undefined  'undefined
    , PropSpec "window.x11.screen"  "WindowX11Screen"   ''Int         'undefined  'undefined
    , PropSpec "window.x11.monitor" "WindowX11Monitor"  ''Int         'undefined  'undefined
    , PropSpec "window.x11.xid"     "WindowX11Xid"      ''Int         'undefined  'undefined
    , PropSpec "application.name"           "ApplicationName"          ''String        'undefined  'undefined
    , PropSpec "application.id"             "ApplicationId"            ''String        'undefined  'undefined
    , PropSpec "application.version"        "ApplicationVersion"       ''String        'undefined  'undefined
    , PropSpec "application.icon"           "ApplicationIcon"          ''B.ByteString  'undefined  'undefined
    , PropSpec "application.icon_name"      "ApplicationIconName"      ''String        'undefined  'undefined
    , PropSpec "application.language"       "ApplicationLanguage"      ''String        'undefined  'undefined
    , PropSpec "application.process.id"     "ApplicationProcessId"     ''Int           'undefined  'undefined
    , PropSpec "application.process.binary" "ApplicationProcessBinary" ''String        'undefined  'undefined
    , PropSpec "application.process.user"   "ApplicationProcessUser"   ''String        'undefined  'undefined
    , PropSpec "application.process.host"   "ApplicationProcessHost"   ''String        'undefined  'undefined
    , PropSpec "application.process.machine_id" "ApplicationProcessMachineId" ''String 'undefined  'undefined
    , PropSpec "application.process.session_id" "ApplicationProcessSessionId" ''Int    'undefined  'undefined
    ]

-- |Generate 'PropTag'
genPropTag :: Q [Dec]
genPropTag =
    let param = mkName "a" in
    return [DataD [] (mkName "PropTag")
        [PlainTV param]
        [ ForallC []
            [EqualP (VarT param) (ConT $ propValueType ps)]
            (NormalC (mkName $ propHaskellName ps) [])
        | ps <- propSpecs
        ] []]

-- |Generate the instance for 'GEq'
deriveGEqPropTag :: Q [Dec]
deriveGEqPropTag =
    return [InstanceD []
        (AppT (ConT ''GEq) (ConT $ mkName "PropTag"))
        [FunD 'geq $
            [ Clause [pat, pat] (NormalB (AppE (ConE 'Just) (ConE 'Refl))) []
            | ps <- propSpecs
            , let pat = ConP (mkName $ propHaskellName ps) []
            ]
            ++
            [ Clause [WildP, WildP] (NormalB $ ConE 'Nothing) []
            ]]]

-- |Generate the instance for 'GCompare'
deriveGComparePropTag :: Q [Dec]
deriveGComparePropTag =
    return [InstanceD []
        (AppT (ConT ''GCompare) (ConT $ mkName "PropTag"))
        [FunD 'gcompare $ concat
            [
                [ Clause [pat, pat] (NormalB (ConE 'GEQ)) []
                , Clause [WildP, pat] (NormalB (ConE 'GLT)) []
                , Clause [pat, WildP] (NormalB (ConE 'GGT)) []
                ]
            | ps <- propSpecs
            , let pat = ConP (mkName $ propHaskellName ps) []
            ]]]

-- |Generate the in manshel function
genToKeyValue :: Q [Dec]
genToKeyValue = do
    let propTag = ConT $ mkName "PropTag"
    let func = mkName "toKeyValue"
    let var = mkName "x"
    sig <- [t|DSum $(return propTag) -> (String, String)|]
    return
        [ SigD func sig
        , FunD func
            [ Clause
                [InfixP pat '(:=>) (VarP var)]
                (NormalB $ TupE
                    [ LitE $ StringL $ propRawName ps
                    , AppE (VarE $ propToRawValue ps) (VarE var)
                    ]) []
            | ps <- propSpecs
            , let pat = ConP (mkName $ propHaskellName ps) []
            ]]

-- |Generate the out manshel function
genFromKeyValue :: Q [Dec]
genFromKeyValue = do
    let propTag = ConT $ mkName "PropTag"
    let func = mkName "fromKeyValue"
    let var = mkName "x"
    sig <- [t|String -> String -> DSum $(return propTag)|]
    return
        [ SigD func sig
        , FunD func $
            [ Clause
                [ LitP $ StringL $ propRawName ps
                , VarP var
                ]
                (NormalB $ InfixE
                    (Just $ ConE $ mkName $ propHaskellName ps)
                    (ConE '(:=>))
                    (Just $ AppE (VarE (propFromRawValue ps)) (VarE var)))
                []
            | ps <- propSpecs
            ]
            ++
            -- XXX: poor error handling
            [Clause
                [WildP, WildP]
                (NormalB $ AppE (VarE 'error)
                    (LitE $ StringL "unknown property name"))
                []]]
