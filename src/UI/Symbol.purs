module UI.Symbol
    ( Action(..)
    , Message
    , SymbolInput
    , State
    , Slot
    , Query(..)
    , component
    , initialState
    ) where

import Prelude
    ( class Eq, class Ord
    , (<$>), (<>), ($), (<<<), (>>=)
    , Unit
    , const, otherwise, bind, discard, pure, unit
    )
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Control.Applicative (when)
import Data.Array as A
import Data.Map as M

import WFF as WFF
import UI.HTMLHelp (select)
import Parser (parseSymbol)
import Symbol
    ( Symbol(..), CustomSymbol(..)
    , makeUnary, makeBinary, getTyped, getDisplay, getOperator, renderableUnary
    , renderableBinary
    )
import UI.Capabilities
    ( class ReadSymbols, class WriteSymbols, class Nav, class Error
    , class ReadNav
    , parse, error, getSymbols, addSymbol, isSymbolWindow, close, getSymbolMap
    )

type Slot = H.Slot Query Message

data Query a = Update a

data Action
    = Name String
    | Def String
    | Type SymbolInput
    | SetAlias String
    | Add
    | Close
    | NoAction

type Message = Unit

data SymbolInput
    = UnaryIn
    | BinaryIn
    | AliasIn

derive instance eqSymbolInput :: Eq SymbolInput
derive instance ordSymbolInput :: Ord SymbolInput

type State =
    { symbols :: Array Symbol
    , open :: Boolean
    , name :: String
    , definition :: String
    , input :: SymbolInput
    , alias :: String
    }

component :: forall m.
    ReadSymbols m =>
    WriteSymbols m =>
    Nav m =>
    Error m =>
    ReadNav m =>
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
    , open : false
    , name : ""
    , definition : ""
    , alias : ""
    , input : BinaryIn
    }

symbolRow :: forall m. Symbol -> H.ComponentHTML Action () m
symbolRow (Custom (UnarySymbol s)) = HH.tr
    []
    [ HH.td [] []
    , HH.td
        [ HP.class_ $ HH.ClassName "symbol-name" ]
        [ HH.text $ WFF.renderUnaryOp s.operator <> "A" ]
    , HH.td
        [ HP.class_ $ HH.ClassName "equiv-symbol" ]
        [ HH.text " ≡ " ]
    , HH.td
        [ HP.class_ $ HH.ClassName "symbol-def" ]
        [ HH.text $ WFF.render $ renderableUnary s.definition ]
    ]
symbolRow (Custom (BinarySymbol s)) = HH.tr
    []
    [ HH.td [] []
    , HH.td
        [ HP.class_ $ HH.ClassName "symbol-name" ]
        [ HH.text $ "A" <> WFF.renderBinaryOp s.operator <> "B" ]
    , HH.td
        [ HP.class_ $ HH.ClassName "equiv-symbol" ]
        [ HH.text " ≡ " ]
    , HH.td
        [ HP.class_ $ HH.ClassName "symbol-def" ]
        [ HH.text $ WFF.render $ renderableBinary s.definition ]
    ]
symbolRow (Alias s) = HH.tr
    []
    [ HH.td [] []
    , HH.td
        [ HP.class_ $ HH.ClassName "symbol-name" ]
        [ HH.text $ getTyped $ Alias s ]
    , HH.td
        [ HP.class_ $ HH.ClassName "equiv-symbol" ]
        [ HH.text " ↦ " ]
    , HH.td
        [ HP.class_ $ HH.ClassName "symbol-def" ]
        [ HH.text $ getDisplay $ getOperator $ Alias s ]
    ]
symbolRow (Builtin s) = HH.tr
    []
    [ HH.td [] []
    , HH.td
        [ HP.class_ $ HH.ClassName "symbol-name" ]
        [ HH.text $ getTyped $ Builtin s ]
    , HH.td
        [ HP.class_ $ HH.ClassName "equiv-symbol" ]
        []
    , HH.td
        [ HP.class_ $ HH.ClassName "symbol-def" ]
        [ HH.text "builtin symbol" ]
    ]

notAlias :: Symbol -> Boolean
notAlias (Alias _) = false
notAlias _ = true

newSymbolRow :: forall m. State -> H.ComponentHTML Action () m
newSymbolRow state = HH.tr
    [ HP.id_ "new-symbol" ]
    [ HH.td
        []
        [ select "operator-select" (Just <<< Type) (const false) state.input
            [ Tuple "Binary" BinaryIn
            , Tuple "Unary" UnaryIn
            , Tuple "Alias" AliasIn
            ]
        ]
    , HH.td
        [ HP.class_ $ HH.ClassName "symbol-name" ]
        $ case state.input of
            BinaryIn ->
                [ HH.text "A"
                , HH.input
                    [ HE.onValueChange $ Just <<< Name
                    , HP.value state.name
                    , HP.id_ "choose-name"
                    ] -- todo make input type text?
                , HH.text "B"
                ]
            UnaryIn ->
                [ HH.input
                    [ HE.onValueChange $ Just <<< Name
                    , HP.value state.name
                    , HP.id_ "choose-name"
                    ] -- todo make input type text?
                , HH.text "A"
                ]
            AliasIn ->
                [ HH.input
                    [ HE.onValueChange $ Just <<< Name
                    , HP.value state.name
                    , HP.id_ "choose-name"
                    ] -- todo make input type text?
                ]
    , HH.td
        [ HP.class_ $ HH.ClassName "equiv-symbol" ]
        [ HH.text $ case state.input of
            AliasIn -> " ↦ "
            _ -> " ≡ "
        ]
    , HH.td
        [ HP.class_ $ HH.ClassName "symbol-def" ]
        [ case state.input of
            AliasIn -> select "alias-select" (Just <<< SetAlias) (const false)
                state.alias $
                    (\x -> Tuple x x) <<< getDisplay <<< getOperator <$>
                    A.filter notAlias state.symbols
            _ -> HH.input
                [ HE.onValueChange $ Just <<< Def
                , HP.value state.definition
                , HP.id_ "set-def"
                ] -- todo make input type text?
        ]
    ]

render :: forall m. State -> H.ComponentHTML Action () m
render state
    | state.open = HH.div
        [ HP.class_ $ HH.ClassName "floating"
        , HP.id_ "symbol-box"
        ]
        [ HH.table
            [ HP.id_ "symbol-list" ]
            $ (symbolRow <$> state.symbols) <> [ newSymbolRow state ]
        , HH.div
            [ HE.onClick $ const $ Just Add
            , HP.id_ "add-symbol"
            , HP.class_ $ HH.ClassName "button"
            ]
            [ HH.text "Add Symbol" ]
        , HH.div
            [ HE.onClick $ const $ Just Close
            , HP.id_ "close-button"
            , HP.class_ $ HH.ClassName "button"
            ]
            [ HH.text "Close" ]
        ]
    | otherwise = HH.div [] []

handleAction :: forall m.
    ReadSymbols m =>
    WriteSymbols m =>
    Nav m =>
    Error m =>
    Action -> H.HalogenM State Action () Message m Unit
handleAction (Name n) = H.modify_ $ _ { name = n }
handleAction (Def d) = H.modify_ $ _ { definition = d }
handleAction (Type t) = H.modify_ $ _ { input = t }
handleAction (SetAlias a) = H.modify_ $ _ { alias = a }
handleAction Add = H.get >>= \state -> case state.input of
    BinaryIn -> parse state.definition >>= \defp ->
        case do
            name <- parseSymbol state.name
            def <- defp
            makeBinary "A" "B" name def
        of
            Left e -> error e
            Right symbol -> do
                success <- addSymbol $ Custom symbol
                when success $ H.modify_ $ _
                    { name = "", definition = "", input = BinaryIn, alias = "" }
    UnaryIn -> parse state.definition >>= \defp ->
        case do
            name <- parseSymbol state.name
            def <- defp
            makeUnary "A" name def
        of
            Left e -> error e
            Right symbol -> do
                success <- addSymbol $ Custom symbol
                when success $ H.modify_ $ _
                    { name = "", definition = "", input = BinaryIn, alias = "" }
    AliasIn -> getSymbolMap >>= \m ->
        case parseSymbol state.name of
            Left e -> error e
            Right name -> case M.lookup state.alias m of
                Nothing -> error "Select a symbol to alias"
                Just operator -> do
                    success <- addSymbol $ Alias { name, operator }
                    when success $ H.modify_ $ _
                        { name = ""
                        , definition = ""
                        , input = BinaryIn
                        , alias = ""
                        }
handleAction Close = close
handleAction NoAction = pure unit

handleQuery :: forall a m.
    ReadSymbols m =>
    ReadNav m =>
    Query a -> H.HalogenM State Action () Message m (Maybe a)
handleQuery (Update a) = do
    symbols <- getSymbols
    open <- isSymbolWindow
    H.modify_ $ _ { symbols = symbols, open = open }
    pure $ Just a
