module UI
    ( run
    ) where

import Prelude
    (Unit, ($), (<>), (*>), (<$>), (==), (>>=), bind, const, unit, pure)
import Effect (Effect)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Effect.Ref as R
import Data.Maybe (Maybe(..))
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem)
import Data.Argonaut.Parser as AP
import Data.Either (Either(..))

import UI.AppState (AppStateM)
import UI.AppState as A
import UI.Sequent as UISeq
import UI.Symbol as UISym
import UI.Proof as UIP
import UI.ImEx as UIE
import UI.Capabilities
    ( class ReadSymbols, class WriteSymbols, class ReadSequents, class ReadNav
    , class WriteSequents, class Nav, class Error, class ReadError
    , class WriteProof, class History, class ReadProof, class ReadFile
    , Window(..)
    , getErrors, setWindow, undo, redo, new, clear, canNew, canUndo, canRedo
    , getWindow
    )
import Json (fromJson)

data Action
    = Update
    | Open Window
    | Undo
    | Redo
    | New
    | ClearError
    | NoAction

type State =
    { window :: Window
    , errors :: Maybe (Array String)
    , ableNew :: Boolean
    , ableUndo :: Boolean
    , ableRedo :: Boolean
    }

type Slots =
    ( sequents :: UISeq.Slot Unit
    , symbols :: UISym.Slot Unit
    , proof :: UIP.Slot Unit
    , imex  :: UIE.Slot Unit
    )

_sequents :: SProxy "sequents"
_sequents = SProxy

_symbols :: SProxy "symbols"
_symbols = SProxy

_proof :: SProxy "proof"
_proof = SProxy

_imex :: SProxy "imex"
_imex = SProxy

component :: forall q. H.Component HH.HTML q Action Unit AppStateM
component = H.mkComponent
    { initialState : const initialState
    , render
    , eval : H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Update
        }
    }

initialState :: State
initialState =
    { window : NoWindow
    , errors : Nothing
    , ableNew : false
    , ableUndo : false
    , ableRedo : false
    }

menu :: forall m. State -> H.ComponentHTML Action Slots m
menu state = HH.div
    [ HP.id_ "menu" ]
    [ HH.div
        [ HP.class_ $ HH.ClassName $
            if state.ableNew then
                "menu-button"
            else
                "menu-button disabled"
        , HP.id_ "new-button"
        , HE.onClick $ const $ Just New
        ]
        [ HH.text "New" ]
    , HH.div
        [ HP.class_ $ HH.ClassName $
            if state.ableUndo then
                "menu-button"
            else
                "menu-button disabled"
        , HP.id_ "undo-button"
        , HE.onClick $ const $ Just Undo
        ]
        [ HH.text "Undo" ]
    , HH.div
        [ HP.class_ $ HH.ClassName $
            if state.ableRedo then
                "menu-button"
            else
                "menu-button disabled"
        , HP.id_ "redo-button"
        , HE.onClick $ const $ Just Redo
        ]
        [ HH.text "Redo" ]
    , HH.div
        [ HP.class_ $ HH.ClassName "menu-button"
        , HP.id_ "symbol-window-button"
        , HE.onClick $ const $ Just $ Open $
            if state.window == SymbolWindow then NoWindow else SymbolWindow
        ]
        [ HH.text "Symbols" ]
    , HH.div
        [ HP.class_ $ HH.ClassName "menu-button"
        , HP.id_ "sequent-window-button"
        , HE.onClick $ const $ Just $ Open $
            if state.window == SequentWindow then NoWindow else SequentWindow
        ]
        [ HH.text "Sequents" ]
    , HH.div
        [ HP.class_ $ HH.ClassName "menu-button"
        , HP.id_ "import-export-window-button"
        , HE.onClick $ const $ Just $ Open $
            if state.window == IEWindow then NoWindow else IEWindow
        ]
        [ HH.text "Import/Export" ]
    ]

render :: forall m.
    ReadSymbols m =>
    WriteSymbols m =>
    ReadSequents m =>
    WriteSequents m =>
    ReadProof m =>
    WriteProof m =>
    Error m =>
    ReadNav m =>
    Nav m =>
    ReadFile m =>
    State -> H.ComponentHTML Action Slots m
render state = HH.div
    [ HP.id_ "main" ] $
    [ menu state
    , HH.slot _sequents unit UISeq.component UISeq.NoAction
        (const $ Just Update)
    , HH.slot _symbols unit UISym.component UISym.NoAction
        (const $ Just Update)
    , HH.slot _proof unit UIP.component UIP.NoAction
        (const $ Just Update)
    , HH.slot _imex unit UIE.component UIE.NoAction
        (const $ Just Update)
    ] <> case state.errors of
        Nothing -> []
        Just errs ->
            [ HH.div
                [ HP.id_ "error"
                , HE.onClick $ const $ Just ClearError
                ]
                $ (\e -> HH.div [] [HH.text e]) <$> errs
            ]

handleAction :: forall m.
    Error m =>
    ReadError m =>
    Nav m =>
    History m =>
    ReadNav m =>
    Action -> H.HalogenM State Action Slots Unit m Unit
handleAction Update = do
    _ <- H.query _sequents unit $ UISeq.Update unit
    _ <- H.query _symbols unit $ UISym.Update unit
    _ <- H.query _proof unit $ UIP.Update unit
    _ <- H.query _imex unit $ UIE.Update unit
    errors <- getErrors
    ableNew <- canNew
    ableUndo <- canUndo
    ableRedo <- canRedo
    window <- getWindow
    H.put {window, errors, ableNew, ableUndo, ableRedo}
handleAction (Open w) = setWindow w *> handleAction Update
handleAction Undo = undo *> handleAction Update
handleAction Redo = redo *> handleAction Update
handleAction New = new *> handleAction Update
handleAction ClearError = clear *> handleAction Update
handleAction NoAction = pure unit

run :: Effect Unit
run = do
    pf <- window >>= localStorage >>= getItem "proof"
    let start = case pf of
            Nothing -> A.start
            Just string -> case AP.jsonParser string of
                Left e -> A.startWithErr $
                    "Error loading from previous session: " <> e
                Right j -> case fromJson j of
                    Left e -> A.startWithErr $
                        "Error loading from previous session: " <> e
                    Right p -> A.startWith p
    ref <- R.new start
    HA.runHalogenAff $ HA.awaitBody >>=
        runUI (H.hoist (A.run ref) component) NoAction
