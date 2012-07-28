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
                                                     , getInfoByTokenCallbackFuncName :: String
                                                     , getInfoByTokenCallbackFuncType :: String
                                                     , foreignFunctionNameByToken :: String
                                                     , wrappedFuncNameByToken :: String
                                                     }

getInfoByIndexFuncSpecs :: [GetInfoByTokenFuncSpec]
getInfoByIndexFuncSpecs =
    [GetInfoByTokenFuncSpec "getSinkInfoByIndex" "sinkInfoCallback" "RawSinkInfoCallback" "contextGetSinkInfoByIndex" "wrappedSinkInfoCallback"
    ,GetInfoByTokenFuncSpec "getCardInfoByIndex" "cardInfoCallback" "RawCardInfoCallback" "contextGetCardInfoByIndex" "wrappedCardInfoCallback"
    ,GetInfoByTokenFuncSpec "getSourceInfoByIndex" "sourceInfoCallback" "RawSourceInfoCallback" "contextGetSourceInfoByIndex" "wrappedSourceInfoCallback"
    ,GetInfoByTokenFuncSpec "getSampleInfoByIndex" "sampleInfoCallback" "RawSampleInfoCallback" "contextGetSampleInfoByIndex" "wrappedSampleInfoCallback"
    ,GetInfoByTokenFuncSpec "getClientInfoByIndex" "clientInfoCallback" "RawClientInfoCallback" "contextGetClientInfo" "wrappedClientInfoCallback"
    ,GetInfoByTokenFuncSpec "getSourceOutputInfoByIndex" "sourceOutputInfoCallback" "RawSourceOutputInfoCallback" "contextGetSourceOutputInfo" "wrappedSourceOutputInfoCallback"
    ,GetInfoByTokenFuncSpec "getModuleInfoByIndex" "moduleInfoCallback" "RawModuleInfoCallback" "contextGetModuleInfo" "wrappedModuleInfoCallback"
    ]

getInfoByNameFuncSpecs :: [GetInfoByTokenFuncSpec]
getInfoByNameFuncSpecs =
    [GetInfoByTokenFuncSpec "getSinkInputInfoByName" "sinkInfoCallback" "RawSinkInfoCallback" "contextGetSinkInfoByName" "wrappedSinkInfoCallback"
    ,GetInfoByTokenFuncSpec "getCardInfoByName" "cardInfoCallback" "RawCardInfoCallback" "contextGetCardInfoByName" "wrappedCardInfoCallback"
    ,GetInfoByTokenFuncSpec "getSourceInfoByName" "sourceInfoCallback" "RawSourceInfoCallback" "contextGetSourceInfoByName" "wrappedSourceInfoCallback"
    ,GetInfoByTokenFuncSpec "getSampleInfoByName" "sampleInfoCallback" "RawSampleInfoCallback" "contextGetSampleInfoByName" "wrappedSampleInfoCallback"
    ]


{-getVariousInfoByIndex :: Context -> Int -> (RawContextPtr -> Int -> t -> Maybe (StablePtr (TVar [a])) -> IO RawOperationPtr) -> t -> IO a-}

{-genGetVariousInfoByXXX = [d| getVariousInfoByIndex ctx idx foreignFunction haskellCallback = mask_ $ bracket (do mon <- newTVarIO []; monPtr <- newStablePtr mon; return (mon, monPtr)) (freeStablePtr . snd) (\(mon, monPtr) -> (let rawCtxPtr = ctxRaw ctx in do { op <- foreignFunction rawCtxPtr idx haskellCallback (Just monPtr); autoWait ctx op; rawVariousInfoList <- readTVarIO mon; return $ head rawVariousInfoList; }))-}
                         {-|]-}


genGetInfoByIndex :: Q [Dec]
genGetInfoByIndex =
    return $ [FunD (mkName $ getInfoByTokenFuncName gifs)
                [Clause [VarP (mkName "ctx"), VarP (mkName "idx")] 
                    (NormalB $ AppE (AppE (AppE (AppE (VarE (mkName "getVariousInfoByIndex")) (VarE (mkName "ctx"))) (VarE (mkName "idx"))) (VarE (mkName $ foreignFunctionNameByToken gifs))) (VarE (mkName $ wrappedFuncNameByToken gifs)))  []
                ]
           | gifs <- getInfoByIndexFuncSpecs
           ]

genGetInfoByName :: Q [Dec]
genGetInfoByName =
    return $ [FunD (mkName $ getInfoByTokenFuncName gifs)
                [Clause [VarP (mkName "ctx"), VarP (mkName "idx")] 
                    (NormalB $ AppE (AppE (AppE (AppE (VarE (mkName "getVariousInfoByName")) (VarE (mkName "ctx"))) (VarE (mkName "idx"))) (VarE (mkName $ foreignFunctionNameByToken gifs))) (VarE (mkName $ wrappedFuncNameByToken gifs)))  []
                ]
           | gifs <- getInfoByNameFuncSpecs
           ]
