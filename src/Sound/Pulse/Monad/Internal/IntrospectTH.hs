{-
This file is part of Pulse, a Haskell binding to PulseAudio library.

Pulse is free software: you can redistribute it and/or modify it under
BSD-3. You should have received a copy of the BSD-3 License along with
Pulse. If not, see <http://www.opensource.org/licenses/BSD-3-clause>.
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

{- |
This module provides Template Haskell generators for 'Introspect'.
-}
module Sound.Pulse.Monad.Internal.IntrospectTH where

import Language.Haskell.TH


data GetInfoFuncSpec = GetInfoFuncSpec { getInfoFuncName :: String
                                       }

getInfoFuncSpecs :: [GetInfoFuncSpec]
getInfoFuncSpecs =
    [GetInfoFuncSpec "getSinkInputInfoList"
    ,GetInfoFuncSpec "getCardInfoList"
    ,GetInfoFuncSpec "getClientInfoList"
    ,GetInfoFuncSpec "getSampleInfoList"
    ,GetInfoFuncSpec "getSourceOutputInfoList"
    ,GetInfoFuncSpec "getModuleInfoList"
    ]

genGetInfoList :: Q [Dec] 
genGetInfoList = 
    return [FunD (mkName $ getInfoFuncName gifs) 
                [Clause [VarP (mkName "ctx")] (NormalB $ VarE $ mkName "ctx") []
                ]
           | gifs <- getInfoFuncSpecs
           ]


data GetInfoByTokenFuncSpec = GetInfoByTokenFuncSpec { getInfoByTokenFuncName :: String
                                                     }

getInfoByTokenFuncSpecs :: [GetInfoByTokenFuncSpec]
getInfoByTokenFuncSpecs =
    [GetInfoByTokenFuncSpec "getSinkInputInfoByIndex"
    ,GetInfoByTokenFuncSpec "getCardInfoByIndex"
    ,GetInfoByTokenFuncSpec "getSourceInfoByIndex"
    ,GetInfoByTokenFuncSpec "getSampleInfoByIndex"
    ,GetInfoByTokenFuncSpec "getClientInfoByIndex"
    ,GetInfoByTokenFuncSpec "getSourceOutputInfoByIndex"
    ,GetInfoByTokenFuncSpec "getModuleInfoByIndex"
    ,GetInfoByTokenFuncSpec "getSinkInputInfoByName"
    ,GetInfoByTokenFuncSpec "getCardInfoByName"
    ,GetInfoByTokenFuncSpec "getSourceInfoByName"
    ,GetInfoByTokenFuncSpec "getSampleInfoByName"
    ]

genGetInfoByToken :: Q [Dec] 
genGetInfoByToken = 
    return [FunD (mkName $ getInfoByTokenFuncName gifs) 
                [Clause [VarP (mkName "ctx")] (NormalB $ VarE $ mkName "ctx") []
                ]
           | gifs <- getInfoByTokenFuncSpecs
           ]
