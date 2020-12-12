module Json.Symbol
    ( toJson
    , fromJson
    , allFromJson
    ) where

import Prelude (($), (<>), (<$>), (&&), bind, pure)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as AC
import Data.Either (Either(..))
import Data.Either as E
import Data.Foldable (foldM)
import Data.Tuple (Tuple(..))
import Foreign.Object as O
import Data.Map as M
import Data.Maybe (Maybe(..))

import Json.WFF as JW
import Parser (parseSymbol)
import Symbol (Symbol(..), SymbolMap, BuiltinSymbol(..))
import Symbol as S
import WFF as WFF

toJson :: Symbol -> Json
toJson (Custom (S.UnarySymbol u)) = AC.fromObject $ O.fromFoldable
    [ Tuple "symbol" $ AC.fromString $ WFF.renderUnaryOp u.operator
    , Tuple "prop" $ AC.fromString "A"
    , Tuple "definition" $ JW.toJson $ S.renderableUnary u.definition
    ]
toJson (Custom (S.BinarySymbol b)) = AC.fromObject $ O.fromFoldable
    [ Tuple "symbol" $ AC.fromString $ WFF.renderBinaryOp b.operator
    , Tuple "propa" $ AC.fromString "A"
    , Tuple "propb" $ AC.fromString "B"
    , Tuple "definition" $ JW.toJson $ S.renderableBinary b.definition
    ]
toJson (Builtin b) = AC.fromObject $ O.fromFoldable
    [ Tuple "symbol" $ AC.fromString $ S.getDisplay $ S.getOperator $ Builtin b
    , Tuple "builtin" $ AC.jsonNull
    ]
toJson (Alias a) = AC.fromObject $ O.fromFoldable
    [ Tuple "symbol" $ AC.fromString $ S.getTyped $ Alias a
    , Tuple "alias" $ AC.fromString $ S.getDisplay $ S.getOperator $ Alias a
    ]

fromObject :: SymbolMap -> O.Object Json -> Either String Symbol
fromObject m o | O.member "prop" o = do
    symJson <- E.note "Symbol is missing name" $ O.lookup "symbol" o
    symbol <- AC.caseJsonString (Left "Symbol name is not a string")
        parseSymbol symJson
    propJson <- E.note "Symbol is missing prop" $ O.lookup "prop" o
    prop <- AC.caseJsonString (Left "Symbol prop is not a string")
        Right propJson
    defJson <- E.note "Symbol is missing definition" $ O.lookup "definition" o
    definition <- JW.fromJson m defJson
    Custom <$> S.makeUnary prop symbol definition
fromObject m o | (O.member "propa" && O.member "propb") o = do
    symJson <- E.note "Symbol is missing name" $ O.lookup "symbol" o
    symbol <- AC.caseJsonString (Left "Symbol name is not a string")
        parseSymbol symJson
    propaJson <- E.note "Symbol is missing propa" $ O.lookup "propa" o
    propa <- AC.caseJsonString (Left "Symbol propa is not a string")
        Right propaJson
    propbJson <- E.note "Symbol is missing propb" $ O.lookup "propb" o
    propb <- AC.caseJsonString (Left "Symbol propb is not a string")
        Right propbJson
    defJson <- E.note "Symbol is missing definition" $ O.lookup "definition" o
    definition <- JW.fromJson m defJson
    Custom <$> S.makeBinary propa propb symbol definition
fromObject m o | O.member "builtin" o = do
    symJson <- E.note "Symbol is missing name" $ O.lookup "symbol" o
    symbol <- AC.caseJsonString (Left "Symbol name is not a string")
        parseSymbol symJson
    case symbol of
        "⊥" -> Right $ Builtin $ BuiltinSymbol $ S.NullaryOperator WFF.falsumOp
        "¬" -> Right $ Builtin $ BuiltinSymbol $ S.UnaryOperator WFF.negOp
        "∧" -> Right $ Builtin $ BuiltinSymbol $ S.BinaryOperator WFF.andOp
        "∨" -> Right $ Builtin $ BuiltinSymbol $ S.BinaryOperator WFF.orOp
        "→" -> Right $ Builtin $ BuiltinSymbol $ S.BinaryOperator WFF.impliesOp
        "∀" -> Right $ Builtin $ BuiltinSymbol $ S.QuantOperator WFF.Forall
        "∃" -> Right $ Builtin $ BuiltinSymbol $ S.QuantOperator WFF.Exists
        _ -> Left $ "Unrecognised builtin symbol: " <> symbol
fromObject m o | O.member "alias" o = do
    symJson <- E.note "Symbol is missing name" $ O.lookup "symbol" o
    symbol <- AC.caseJsonString (Left "Symbol name is not a string")
        parseSymbol symJson
    aliasJson <- E.note "Symbol is missing alias" $ O.lookup "alias" o
    alias <- AC.caseJsonString (Left "Symbol alias is not a string")
        parseSymbol aliasJson
    case M.lookup alias m of
        Nothing -> Left "Symbol alias not found"
        Just operator -> Right $ Alias { name : symbol, operator }
fromObject _ _ = Left "Symbol is missing propositions, builtin, or alias"

fromJson :: SymbolMap -> Json -> Either String Symbol
fromJson m = AC.caseJsonObject (Left "Symbol definition is not an object") $
    fromObject m

type Symbols = { symbols :: Array Symbol, symbolMap :: SymbolMap }

addOne :: Symbols -> Json -> Either String Symbols
addOne s j = do
    newSym <- fromJson s.symbolMap j
    newMap <- S.updateMap s.symbolMap newSym
    pure { symbols : s.symbols <> [newSym], symbolMap : newMap }

allFromJson :: Json -> Either String Symbols
allFromJson = AC.caseJsonArray (Left "Symbols are not in a list")
    (foldM addOne { symbols : [], symbolMap : M.empty })
