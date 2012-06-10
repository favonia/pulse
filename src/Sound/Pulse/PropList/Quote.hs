{-# LANGUAGE TemplateHaskell #-}

module Sound.Pulse.PropList.Quote (prop) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import qualified Data.ConfigFile.Parser as DCP
import Data.ConfigFile.Types
import Data.Generics

runParser :: String -> DCP.ParseOutput
runParser s = case (DCP.parse_string s) of
                Left _ -> []
                Right r -> r

quotePropExp :: String -> TH.ExpQ
quotePropExp s = do let pout = runParser s
                    dataToExpQ (const Nothing `extQ` antiPropExp) pout

antiPropExp :: DCP.ParseOutput -> Maybe (TH.Q TH.Exp)
antiPropExp _ = Nothing

quotePropPat :: String -> TH.PatQ
quotePropPat s = do let pout = runParser s
                    dataToPatQ (const Nothing `extQ` antiPropPat) pout

antiPropPat :: DCP.ParseOutput -> Maybe (TH.Q TH.Pat)
antiPropPat _ = Nothing

prop :: QuasiQuoter
prop = QuasiQuoter quotePropExp quotePropPat undefined undefined

