module UI.Proof
    ( PartialDeduction
    , Action(..)
    , Message
    , State
    , Slot
    , Query(..)
    , component
    , initialState
    ) where

import Prelude
    ( class Eq, class Ord, Unit
    , ($), (<>), (==), (<$>), (+), (<<<), (/=)
    , bind, show, pure, discard, const, unit
    )
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Data.Either (Either(..))
import Data.String.Utils as SU
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.Array as A
import Data.Either as E
import Data.Set as Set
import Data.Foldable (length)
import Data.Traversable (traverse)
import Data.Int (fromString)
import Data.Tuple (Tuple(..))
import Control.Applicative (when)

import WFF as W
import UI.HTMLHelp (select)
import Deduction (DeductionRule(..))
import Proof (Deduction(..), Proof(..))
import Proof as P
import Symbol (Symbol(..), CustomSymbol, getDisplay, getOperator)
import Sequent (Sequent)
import Sequent as Seq
import UI.Capabilities
    ( class ReadSymbols, class ReadSequents, class ReadProof, class WriteProof
    , class Error
    , getSymbols, getSequents, getProof, addDeduction, error, parse
    )

type Slot = H.Slot Query Message

data Query a = Update a

data PartialDeduction
    = Full DeductionRule
    | PartSymbol
    | PartSequent

derive instance eqPartialDeduction :: Eq PartialDeduction
derive instance ordPartialDeduction :: Ord PartialDeduction

toDeduction :: PartialDeduction -> Either String DeductionRule
toDeduction (Full d) = Right d
toDeduction PartSymbol = Left "Select a symbol or other reason"
toDeduction PartSequent = Left "Select a sequent or other reason"

makePartial :: PartialDeduction -> PartialDeduction
makePartial (Full (Definition _ _)) = PartSymbol
makePartial (Full (Introduction _ _)) = PartSequent
makePartial d = d

data Action
    = Assumptions String
    | Formula String
    | Reason PartialDeduction
    | References String
    | AddLine
    | NoAction

type Message = Unit

type State =
    { symbols :: Array (Tuple Int CustomSymbol)
    , sequents :: Array (Sequent String String String)
    , proof :: Proof
    , assumptions :: String
    , formula :: String
    , reason :: PartialDeduction
    , references :: String
    }

component :: forall m.
    ReadSymbols m =>
    ReadSequents m =>
    ReadProof m =>
    WriteProof m =>
    Error m =>
    H.Component HH.HTML Query Action Message m
component = H.mkComponent
    { initialState : const initialState
    , render
    , eval : H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        }
    }

initialState :: State
initialState =
    { symbols : []
    , sequents : []
    , proof : P.empty
    , assumptions : ""
    , formula : ""
    , reason : Full Assumption
    , references : ""
    }

renderNewLine :: forall m. State -> H.ComponentHTML Action () m
renderNewLine state = let Proof proof = state.proof in HH.tr
    [ HP.id_ "new-line" ]
    [ HH.td
        [ HP.class_ $ HH.ClassName "line-number" ]
        [ HH.text $ "(" <> show (length proof.lines + 1) <> ")" ]
    , HH.td
        [ HP.class_ $ HH.ClassName "assumptions" ]
        [ HH.input
            [ HE.onValueChange $ Just <<< Assumptions
            , HP.value state.assumptions
            , HP.id_ "assumptions-input"
            ] -- todo make input type text?
        ]
    , HH.td
        [ HP.class_ $ HH.ClassName "formula" ]
        [ HH.input
            [ HE.onValueChange $ Just <<< Formula
            , HP.id_ "formula-input"
            , HP.value state.formula
            ] -- todo make type text?
        ]
    , HH.td
        [ HP.class_ $ HH.ClassName "reason" ]
        [ select "reason-dropdown" (Just <<< Reason) (const false)
            (makePartial state.reason)
                [ Tuple "Assump. I" $ Full Assumption
                , Tuple "→E" $ Full ModusPonens
                , Tuple "→I" $ Full ConditionalProof
                , Tuple "∧I" $ Full AndIntroduction
                , Tuple "∧E" $ Full AndElimination
                , Tuple "∨I" $ Full OrIntroduction
                , Tuple "∨E" $ Full OrElimination
                , Tuple "¬E" $ Full NegationElimination
                , Tuple "¬I" $ Full NegationIntroduction
                , Tuple "RA" $ Full RAA
                , Tuple "⊥" $ Full Falsum
                , Tuple "Def" PartSymbol
                , Tuple "SI" PartSequent
                , Tuple "∀I" $ Full UniversalIntroduction
                , Tuple "∀E" $ Full UniversalElimination
                , Tuple "∃I" $ Full ExistentialIntroduction
                , Tuple "∃E" $ Full ExistentialElimination
                ]
        , HH.div
            ([ HP.id_ "input-dropdown"
            ] <> case state.reason of
                PartSymbol -> []
                Full (Definition _ _) -> []
                PartSequent -> []
                Full (Introduction _ _)  -> []
                _ -> [ HP.class_ $ HH.ClassName "short" ])
            [ case state.reason of
                PartSymbol -> select "symbol-dropdown"
                    (Just <<< Reason) (const false) PartSymbol $
                        (\(Tuple i sym) -> Tuple
                            (getDisplay $ getOperator $ Custom sym) $
                            Full $ Definition sym i)
                        <$> state.symbols
                Full d@(Definition _ _) -> select "symbol-dropdown"
                    (Just <<< Reason) (const false) (Full d) $
                        (\(Tuple i sym) -> Tuple
                            (getDisplay $ getOperator $ Custom sym) $
                            Full $ Definition sym i)
                        <$> state.symbols
                PartSequent -> select "sequent-dropdown"
                    (Just <<< Reason) (const false) PartSequent $
                        A.mapWithIndex (\i seq ->
                            Tuple (Seq.render seq) $ Full $ Introduction seq i)
                        state.sequents
                Full d@(Introduction _ _) -> select "sequent-dropdown"
                    (Just <<< Reason) (const false) (Full d) $
                        A.mapWithIndex (\i seq ->
                            Tuple (Seq.render seq) $ Full $ Introduction seq i)
                        state.sequents
                _ -> HH.text ""
            ]
        , HH.input $
            [ -- todo make type text?
              HE.onValueChange $ Just <<< References
            , HP.value state.references
            , HP.id_ "reference-input"
            ] <> case state.reason of
                PartSymbol -> [ ]
                Full (Definition _ _) -> [ ]
                PartSequent -> [ ]
                Full (Introduction _ _)  -> [ ]
                _ -> [ HP.class_ $ HH.ClassName "long" ]
        ]
    ]

renderDeduction :: forall a m. Int -> Deduction -> H.ComponentHTML a () m
renderDeduction i (Deduction ded) = HH.tr
    []
    [ HH.td
        [ HP.class_ $ HH.ClassName "line-number" ]
        [ HH.text $ "(" <> show (i+1) <> ")" ]
    , HH.td
        [ HP.class_ $ HH.ClassName "assumptions" ]
        [ HH.text $ S.joinWith "," $
            show <$> Set.toUnfoldable ded.assumptions ]
    , HH.td
        [ HP.class_ $ HH.ClassName "formula" ]
        [ HH.text $ W.render ded.deduction ]
    , HH.td
        [ HP.class_ $ HH.ClassName "reason" ]
        [ HH.text $ P.renderReason $ Deduction ded ]
    ]

lineHeading :: forall a m. H.ComponentHTML a () m
lineHeading = HH.tr
    []
    [ HH.th
        [ HP.class_ $ HH.ClassName "line-number" ]
        [ HH.text "Line No." ]
    , HH.th
        [ HP.class_ $ HH.ClassName "assumptions" ]
        [ HH.text "Assumptions" ]
    , HH.th
        [ HP.class_ $ HH.ClassName "formula" ]
        [ HH.text "Formula" ]
    , HH.th
        [ HP.class_ $ HH.ClassName "reason" ]
        [ HH.text "Reason" ]
    ]

render :: forall m. State -> H.ComponentHTML Action () m
render state = let Proof proof = state.proof in HH.div
    [ HP.id_ "proof-box" ]
    [ HH.table
        [ HP.id_ "proof-lines" ]
        $ [ lineHeading ]
            <> A.mapWithIndex renderDeduction proof.lines
            <> [ renderNewLine state ]
    , HH.button
        [ HE.onClick $ const $ Just AddLine
        , HP.id_ "add-line"
        ]
        [ HH.text "Add Line" ]
    ]

commastospaces :: String -> String
commastospaces = SU.mapChars (\x -> if x == "," then " " else x)

handleAction :: forall m.
    ReadSymbols m =>
    WriteProof m =>
    Error m =>
    Action -> H.HalogenM State Action () Message m Unit
handleAction (Assumptions s) = H.modify_ $ _ { assumptions = s }
handleAction (Formula s) = H.modify_ $ _ { formula = s }
handleAction (Reason r) = H.modify_ $ _ { reason = r }
handleAction (References s) = H.modify_ $ _ { references = s }
handleAction AddLine = do
    state <- H.get
    formp <- parse state.formula
    case do
        assumptions <- E.note "Assumptions are not integers" $
            Set.fromFoldable <$> traverse fromString
            (A.filter (_ /= "") $ SU.words $ commastospaces state.assumptions)
        deduction <- formp
        rule <- toDeduction state.reason
        reasons <- E.note "References are not integers" $
            A.sort <$> traverse fromString
            (A.filter (_ /= "") $ SU.words $ commastospaces state.references)
        pure $ Deduction {assumptions, deduction, rule, reasons}
    of
        Left e -> error e
        Right d -> do
            success <- addDeduction d
            when success $ H.modify_ $ _
                { assumptions = ""
                , formula = ""
                , reason =Full Assumption
                , references = ""
                }

handleAction NoAction = pure unit

fromCustom :: Tuple Int Symbol -> Maybe (Tuple Int CustomSymbol)
fromCustom (Tuple i (Custom s)) = Just $ Tuple i s
fromCustom _ = Nothing

handleQuery :: forall a m.
    ReadSymbols m =>
    ReadSequents m =>
    ReadProof m =>
    Query a -> H.HalogenM State Action () Message m (Maybe a)
handleQuery (Update a) = do
    symbols <- A.mapMaybe fromCustom <<< A.mapWithIndex Tuple <$>
        getSymbols
    sequents <- getSequents
    proof <- getProof
    H.modify_ $ _ { symbols = symbols, sequents = sequents, proof = proof }
    pure $ Just a
