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
                                       , getInfoCallbackFuncName :: String
                                       , getInfoCallbackFuncType :: String
                                       , foreignFunctionName :: String
                                       , wrappedFuncName :: String
                                       }

getInfoFuncSpecs :: [GetInfoFuncSpec]
getInfoFuncSpecs =
    [GetInfoFuncSpec "getSinkInfoList" "sinkInfoCallback" "RawSinkInfoCallback" "contextGetSinkInfoList" "wrappedSinkInfoCallback"
    ,GetInfoFuncSpec "getSinkInputInfoList" "sinkInputInfoCallback" "RawSinkInputInfoCallback" "contextGetSinkInputInfoList" "wrappedSinkInputInfoCallback"
    ,GetInfoFuncSpec "getCardInfoList" "cardInfoCallback" "RawCardInfoCallback" "contextGetCardInfoList" "wrappedCardInfoCallback"
    ,GetInfoFuncSpec "getClientInfoList" "clientInfoCallback" "RawClientInfoCallback" "contextGetClientInfoList" "wrappedClientInfoCallback"
    ,GetInfoFuncSpec "getSampleInfoList" "sampleInfoCallback" "RawSampleInfoCallback" "contextGetSampleInfoList" "wrappedSampleInfoCallback"
    ,GetInfoFuncSpec "getSourceInfoList" "sourceInfoCallback" "RawSourceInfoCallback" "contextGetSourceInfoList" "wrappedSourceInfoCallback"
    ,GetInfoFuncSpec "getSourceOutputInfoList" "sourceOutputInfoCallback" "RawSourceOutputInfoCallback" "contextGetSourceOutputInfoList" "wrappedSourceOutputInfoCallback"
    ,GetInfoFuncSpec "getModuleInfoList" "moduleInfoCallback" "RawModuleInfoCallback" "contextGetModuleInfoList" "wrappedModuleInfoCallback"
    ]


genVariousInfoListCallback :: Q [Dec] 
genVariousInfoListCallback =
    return $ 
           [SigD (mkName $ getInfoCallbackFuncName gifs) 
                 (ForallT [PlainTV $ mkName "a"] [] 
                    (AppT (ConT $ mkName $ getInfoCallbackFuncType gifs) (VarT $ mkName $ "a")))
           | gifs <- getInfoFuncSpecs 
           ] ++
           [
            FunD (mkName $ getInfoCallbackFuncName gifs)
                [Clause [] (NormalB $ VarE $ mkName "variousInfoListCallbackHelper") []
                ]
           | gifs <- getInfoFuncSpecs
           ]


genGetInfoList :: Q [Dec]
genGetInfoList =
    return $ [FunD (mkName $ getInfoFuncName gifs)
                [Clause [VarP (mkName "ctx")] 
                    (NormalB $ AppE (AppE (AppE (VarE (mkName "getVariousInfoList")) (VarE (mkName "ctx"))) (VarE (mkName $ foreignFunctionName gifs))) (VarE (mkName $ wrappedFuncName gifs)))  []
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
