module Json.Proof
    ( toJson
    , fromJson
    ) where

import Prelude (($), (<<<), (<$>), (>=>), pure, bind, flip)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as AC
import Foreign.Object as O
import Data.Tuple (Tuple(..))
import Data.Either (Either)
import Data.Either as E
import Data.Int (toNumber, fromNumber)
import Data.Set as S
import Data.Traversable (traverse)
import Data.Foldable (foldM)
import Data.Array (sort)

import Json.WFF as JW
import Json.Deduction as JD
import Proof as P
import Proof (Deduction(..), Proof(..))
import Symbol (Symbol, SymbolMap)
import Sequent (Sequent)

fromDeduction :: Deduction -> Json
fromDeduction (Deduction d) = AC.fromObject $ O.fromFoldable
    [ Tuple "assumptions" $ AC.fromArray $
        AC.fromNumber <<< toNumber <$> S.toUnfoldable d.assumptions
    , Tuple "formula" $ JW.toJson d.deduction
    , Tuple "rule" $ JD.toJson d.rule
    , Tuple "references" $ AC.fromArray $
        AC.fromNumber <<< toNumber <$> d.reasons
    ]

toDeduction :: SymbolMap -> Array Symbol -> Array (Sequent String String String)
    -> Proof -> Json -> Either String Deduction
toDeduction symbolMap syms seqs (Proof p) j = do
    o <- E.note "Deduction is not an object" $ AC.toObject j
    formJson <- E.note "Deduction is missing formula" $ O.lookup "formula" o
    deduction <- JW.fromJson symbolMap formJson
    ruleJson <- E.note "Deduction is missing rule" $ O.lookup "rule" o
    rule <- JD.fromJson syms seqs ruleJson
    refJson <- E.note "Deduction is missing references" $
        O.lookup "references" o
    refArr <- E.note "Deduction references are not a list" $
        AC.toArray refJson
    reasons <- E.note "Deduction references are not integers" $
        sort <$>
        traverse (AC.toNumber >=> fromNumber) refArr
    assJson <- E.note "Deduction is missing assumptions" $
        O.lookup "assumptions" o
    assArr <- E.note "Deduction assumptions are not a list" $
        AC.toArray assJson
    assumptions <- E.note "Deduction assumptions are not integers" $
        S.fromFoldable <$>
        traverse (AC.toNumber >=> fromNumber) assArr
    pure $ Deduction { assumptions, deduction, rule, reasons }

toJson :: Proof -> Json
toJson (Proof p) = AC.fromArray $ fromDeduction <$> p.lines

fromJson :: SymbolMap -> Array Symbol -> Array (Sequent String String String) ->
    Json -> Either String Proof
fromJson symbolMap syms seqs j = do
    linesArr <- E.note "Proof is not a list" $ AC.toArray j
    foldM
        (\p -> toDeduction symbolMap syms seqs p >=> flip P.addDeduction p)
        P.empty
        linesArr
