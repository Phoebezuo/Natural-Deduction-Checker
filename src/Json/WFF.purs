module Json.WFF
    ( toJson
    , fromJson
    ) where

import Prelude ((<<<), ($))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as AC
import Data.Either (Either(..))

import WFF (WFF, render)
import Parser (parse)
import Symbol (SymbolMap)

toJson :: WFF String String String -> Json
toJson = AC.fromString <<< render

fromJson :: SymbolMap -> Json -> Either String (WFF String String String)
fromJson m = AC.caseJsonString (Left "Formula is not a string") $ parse m
