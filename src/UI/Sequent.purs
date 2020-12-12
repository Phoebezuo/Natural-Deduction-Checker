module UI.Sequent
    ( Action(..)
    , Message
    , State
    , Slot
    , Query(..)
    , component
    , initialState
    ) where

import Prelude
    ( Unit
    , ($), (<$>), (<>), (<<<)
    , bind, pure, otherwise, discard, unit, const
    )
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Data.String as S
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Control.Applicative (when)

import WFF as WFF
import Sequent (Sequent(..))
import UI.Capabilities
    ( class ReadSymbols, class ReadSequents, class WriteSequents, class Nav
    , class Error, class ReadNav
    , parse, parseMany, error, addSequent, isSequentWindow, getSequents, close
    )

type Slot = H.Slot Query Message

data Query a = Update a

data Action
    = Ante String
    | Conse String
    | Add
    | Close
    | NoAction

type Message = Unit

type State =
    { sequents :: Array (Sequent String String String)
    , open :: Boolean
    , ante :: String
    , conse :: String
    }

component :: forall m.
    ReadSymbols m =>
    WriteSequents m =>
    Error m =>
    Nav m =>
    ReadSequents m =>
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
    { sequents : []
    , open : false
    , ante : ""
    , conse : ""
    }

renderNewSeq :: forall m. State -> H.ComponentHTML Action () m
renderNewSeq state = HH.tr
    [ HP.id_ "new-sequent" ]
    [ HH.td
        [ HP.class_ $ HH.ClassName "ante" ]
        [ HH.input
            [ HE.onValueChange $ Just <<< Ante
            , HP.value state.ante
            , HP.id_ "ante-input"
            ] -- todo make input type text?
        ]
    , HH.td
        [ HP.class_ $ HH.ClassName "seq-symbol" ]
        [ HH.text " ⊢ " ]
    , HH.td
        [ HP.class_ $ HH.ClassName "conse" ]
        [ HH.input
            [ HE.onValueChange $ Just <<< Conse
            , HP.value state.conse
            , HP.id_ "conse-input"
            ] -- todo make input type text?
        ]
    ]

renderSequent :: forall a m.
    Sequent String String String -> H.ComponentHTML a () m
renderSequent (Sequent seq) = HH.tr
    []
    [ HH.td
        [ HP.class_ $ HH.ClassName "ante" ]
        [ HH.text $ S.joinWith "," (WFF.render <$> seq.ante) ]
    , HH.td
        [ HP.class_ $ HH.ClassName "seq-symbol" ]
        [ HH.text " ⊢ " ]
    , HH.td
        [ HP.class_ $ HH.ClassName "conse" ]
        [ HH.text $ WFF.render seq.conse ]
    ]

render :: forall m. State -> H.ComponentHTML Action () m
render state
    | state.open = HH.div
        [ HP.class_ $ HH.ClassName "floating"
        , HP.id_ "sequent-box"
        ]
        [ HH.table
            [ HP.id_ "sequent-list" ]
            $ (renderSequent <$> state.sequents) <> [ renderNewSeq state ]
        , HH.div
            [ HE.onClick $ const $ Just Add
            , HP.id_ "add-sequent"
            , HP.class_ $ HH.ClassName "button"
            ]
            [ HH.text "Add Sequent" ]
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
    WriteSequents m =>
    Error m =>
    Nav m =>
    Action -> H.HalogenM State Action () Message m Unit
handleAction (Ante s) = H.modify_ $ _ { ante = s }
handleAction (Conse s) = H.modify_ $ _ { conse = s }
handleAction Add = do
    state <- H.get
    antesp <- parseMany state.ante
    consep <- parse state.conse
    case do
        ante <- antesp
        conse <- consep
        pure $ Sequent { ante, conse }
    of
        Left e -> error e
        Right s -> do
            success <- addSequent s
            when success $ H.modify_ $ _ { ante = "", conse = "" }
handleAction Close = close
handleAction NoAction = pure unit

handleQuery :: forall a m.
    ReadSequents m =>
    ReadNav m =>
    Query a -> H.HalogenM State Action () Message m (Maybe a)
handleQuery (Update a) = do
    sequents <- getSequents
    open <- isSequentWindow
    H.modify_ $ _ { sequents = sequents, open = open }
    pure $ Just a
