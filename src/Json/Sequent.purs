module Json.Sequent
    ( toJson
    , fromJson
    ) where

import Prelude (($), (<$>), bind, pure)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as AC
import Foreign.Object as O
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Either as E
import Data.Traversable (traverse)

import Json.WFF as JW
import Sequent (Sequent(..))
import Symbol (SymbolMap)

toJson :: Sequent String String String -> Json
toJson (Sequent s) = AC.fromObject $ O.fromFoldable
    [ Tuple "ante" $ AC.fromArray $ JW.toJson <$> s.ante
    , Tuple "conse" $ JW.toJson $ s.conse
    ]

fromObject :: SymbolMap -> O.Object Json ->
    Either String (Sequent String String String)
fromObject m o = do
    anteJson <- E.note "Sequent is missing ante" $ O.lookup "ante" o
    ante <- AC.caseJsonArray (Left "Sequent ante is not a list")
        (traverse $ JW.fromJson m) anteJson
    conseJson <- E.note "Sequent is missing conse" $ O.lookup "conse" o
    conse <- JW.fromJson m conseJson
    pure $ Sequent { ante, conse }

fromJson :: SymbolMap -> Json -> Either String (Sequent String String String)
fromJson m = AC.caseJsonObject (Left "Sequent is not an object") $ fromObject m
