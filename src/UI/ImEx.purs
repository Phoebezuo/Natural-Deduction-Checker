module UI.ImEx
    ( Action(..)
    , Message
    , State
    , Slot
    , Query(..)
    , component
    , initialState
    ) where

import Prelude (Unit, const, ($), (<<<), (<>), bind, pure, unit, discard)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Data.Maybe (Maybe(..))
import DOM.HTML.Indexed.InputAcceptType (mediaType)
import Data.MediaType.Common (applicationJSON)
import Web.Event.Event (Event)
import Data.Argonaut.Core (stringify)

import Sequent (Sequent)
import Symbol (Symbol)
import Proof (Proof)
import Proof as P
import Json (toJson)
import UI.Capabilities
    ( class Nav, class ReadNav, class ReadFile, class ReadProof
    , class ReadSymbols, class ReadSequents
    , readFile, close, isIEWindow, getSymbols, getSequents, getProof
    )

type Slot = H.Slot Query Message

data Query a = Update a

data Action
    = Close
    | Load Event
    | NoAction

type Message = Unit

type State =
    { open :: Boolean
    , sequents :: Array (Sequent String String String)
    , symbols :: Array Symbol
    , proof :: Proof
    }

component :: forall m.
    Nav m =>
    ReadNav m =>
    ReadFile m =>
    ReadSymbols m =>
    ReadSequents m =>
    ReadProof m =>
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
    { open : false
    , sequents : []
    , symbols : []
    , proof : P.empty
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state | state.open = HH.div
    [ HP.class_ $ H.ClassName "floating"
    , HP.id_ "import-export-box"
    ]
    [ HH.div
        [ HP.id_ "json-input" ]
        [ HH.text "Upload proof: "
        , HH.input
            [ HE.onInput $ Just <<< Load
            , HP.type_ HP.InputFile
            , HP.accept $ mediaType applicationJSON
            ]
        ]
    , HH.div
        [ HP.id_ "json-output" ]
        [ HH.text "Download proof: "
        , HH.a
            [ HP.href $ "data:text/plain;charset=utf-8," <>
                stringify (toJson state.symbols state.sequents state.proof)
            , HP.download "proof.json"
            ]
            [ HH.text "Click here" ]
        ]
    , HH.div
        [ HE.onClick $ const $ Just Close
        , HP.id_ "close-button"
        , HP.class_ $ HH.ClassName "button"
        ]
        [ HH.text "Close" ]
    ]
render _ = HH.div [] []

handleAction :: forall m.
    Nav m =>
    ReadFile m =>
    Action -> H.HalogenM State Action () Message m Unit
handleAction Close = close
handleAction (Load e) = readFile e
handleAction NoAction = pure unit

handleQuery :: forall a m.
    ReadNav m =>
    ReadSymbols m =>
    ReadSequents m =>
    ReadProof m =>
    Query a -> H.HalogenM State Action () Message m (Maybe a)
handleQuery (Update a) = do
    open <- isIEWindow
    sequents <- getSequents
    symbols <- getSymbols
    proof <- getProof
    H.put {open, sequents, symbols, proof}
    pure $ Just a
