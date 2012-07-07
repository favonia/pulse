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
import Data.Typeable
import Data.Dependent.Sum
import Data.GADT.Compare
import Data.GADT.Show
import Language.Haskell.TH
import System.Posix.Types

-- | Specification of a property.
data PropSpec = PropSpec
    { propRawName :: String
    , propHaskellName :: String
    , propValueType :: Name
    , propToRawValue :: Name
    , propFromRawValue :: Name
    }

-- | Access mode. Used in 'DeviceAccessMode'.
data AccessMode = Mmap | MmapRewrite | Serial deriving (Eq, Show, Ord)

-- | Bus type. Used in 'DeviceBus'.
data Bus = Isa | Pci | Usb | Firewire | Bluetooth deriving (Eq, Show, Ord)

-- | Class of a device. Used in 'DeviceClass'.
data Class = Sound | Modem | Monitor | Filter deriving (Eq, Show, Ord)

-- | Form factor. Used in 'DeviceFormFactor'.
data FormFactor = Internal | Speaker | Handset | Tv | Webcam
                | Microphone | Headset | Headphone | HandsFree
                | Car | Hifi | Computer | Portable
    deriving (Eq, Show, Ord)

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
    [ PropSpec "media.name"         "MediaName"         ''String      'id  'id
    , PropSpec "media.title"        "MediaTitle"        ''String      'id  'id
    , PropSpec "media.artist"       "MediaArtist"       ''String      'id  'id
    , PropSpec "media.copyright"    "MediaCopyright"    ''String      'id  'id
    , PropSpec "media.software"     "MediaSoftware"     ''String      'id  'id
    , PropSpec "media.language"     "MediaLanguage"     ''String      'id  'id
    , PropSpec "media.filename"     "MediaFilename"     ''String      'id  'id
    , PropSpec "media.icon_name"    "MediaIconName"     ''String      'id  'id
    , PropSpec "media.role"         "MediaRole"         ''Role        'toRawRole  'fromRawRole
    , PropSpec "filter.want"        "FilterWant"        ''String      'id  'id
    , PropSpec "filter.apply"       "FilterApply"       ''String      'id  'id
    , PropSpec "filter.suppress"    "FilterSuppress"    ''String      'id  'id
    , PropSpec "event.id"           "EventId"           ''String      'id  'id
    , PropSpec "event.description"  "EventDescription"  ''String      'id  'id
    , PropSpec "event.mouse.x"      "EventMouseX"       ''Int         'show  'read
    , PropSpec "event.mouse.y"      "EventMouseY"       ''Int         'show  'read
    , PropSpec "event.mouse.hpos"   "EventMouseHpos"    ''Double      'show  'read
    , PropSpec "event.mouse.vpos"   "EventMouseVpos"    ''Double      'show  'read
    , PropSpec "event.mouse.button" "EventMouseButton"  ''MouseButton 'toRawMouseButton  'fromRawMouseButton
    , PropSpec "window.name"        "WindowName"        ''String      'id  'id
    , PropSpec "window.id"          "WindowId"          ''String      'id  'id
    , PropSpec "window.icon_name"   "WindowIconName"    ''String      'id  'id
    , PropSpec "window.x"           "WindowX"           ''Int         'show  'read
    , PropSpec "window.y"           "WindowY"           ''Int         'show  'read
    , PropSpec "window.width"       "WindowWidth"       ''Int         'show  'read
    , PropSpec "window.height"      "WindowHeight"      ''Int         'show  'read
    , PropSpec "window.hpos"        "WindowHpos"        ''Double      'show  'read
    , PropSpec "window.vpos"        "WindowVpos"        ''Double      'show  'read
    , PropSpec "window.desktop"     "WindowDesktop"     ''Desktop     'toRawDesktop  'fromRawDesktop
    , PropSpec "window.x11.display" "WindowX11Display"  ''String      'id  'id
    , PropSpec "window.x11.screen"  "WindowX11Screen"   ''Int         'show  'read
    , PropSpec "window.x11.monitor" "WindowX11Monitor"  ''Int         'show  'read
    , PropSpec "window.x11.xid"     "WindowX11Xid"      ''Int         'show  'read
    , PropSpec "application.name"           "ApplicationName"          ''String        'id  'id
    , PropSpec "application.id"             "ApplicationId"            ''String        'id  'id
    , PropSpec "application.version"        "ApplicationVersion"       ''String        'id  'id
    , PropSpec "application.icon_name"      "ApplicationIconName"      ''String        'id  'id
    , PropSpec "application.language"       "ApplicationLanguage"      ''String        'id  'id
#if !defined(mingw32_HOST_OS)
    , PropSpec "application.process.id"     "ApplicationProcessId"     ''ProcessID     'show  'read
#endif
    , PropSpec "application.process.binary" "ApplicationProcessBinary" ''String        'id  'id
    , PropSpec "application.process.user"   "ApplicationProcessUser"   ''String        'id  'id
    , PropSpec "application.process.host"   "ApplicationProcessHost"   ''String        'id  'id
    , PropSpec "application.process.machine_id" "ApplicationProcessMachineId" ''String 'id  'id
    , PropSpec "application.process.session_id" "ApplicationProcessSessionId" ''Int    'show  'read
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

-- | Generate the in marshaller for 'PropList'.
genToKeyValue :: Q [Dec]
genToKeyValue = do
    let propTag = ConT $ mkName "PropTag"
    let dsumPropTag = ConT ''DSum `AppT` propTag
    let string2 = TupleT 2 `AppT` ConT ''String `AppT` ConT ''String
    let func = mkName "toKeyValue"
    let var = mkName "x"
    return
        [ SigD func $ ArrowT `AppT` dsumPropTag `AppT` string2
        , FunD func
            [ Clause
                [InfixP pat '(:=>) (VarP var)]
                (NormalB $ TupE
                    [ LitE $ StringL $ propRawName ps
                    , VarE (propToRawValue ps) `AppE` VarE var
                    ]) []
            | ps <- propSpecs
            , let pat = ConP (mkName $ propHaskellName ps) []
            ]]

-- | Generate the out marshaller for 'PropList'.
genFromKeyValue :: Q [Dec]
genFromKeyValue = do
    let propTag = ConT $ mkName "PropTag"
    let dsumPropTag = ConT ''DSum `AppT` propTag
    let stringArrow = (ArrowT `AppT` ConT ''String `AppT`)
    let func = mkName "fromKeyValue"
    let var = mkName "x"
    return
        [ SigD func $ stringArrow $ stringArrow dsumPropTag
        , FunD func $
            [ Clause
                [ LitP $ StringL $ propRawName ps
                , VarP var
                ]
                (NormalB $ InfixE
                    (Just $ ConE $ mkName $ propHaskellName ps)
                    (ConE '(:=>))
                    (Just $ VarE (propFromRawValue ps) `AppE` VarE var))
                []
            | ps <- propSpecs
            ]
            ++
            -- XXX: poor error handling
            [Clause
                [WildP, WildP]
                (NormalB $ VarE 'error `AppE` LitE (StringL "unknown property name"))
                []]]
