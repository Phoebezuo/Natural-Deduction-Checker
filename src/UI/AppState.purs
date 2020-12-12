module UI.AppState
    ( ProofState
    , AppState
    , AppStateM
    , run
    , start
    , startWith
    , startWithErr
    ) where

import Prelude
    ( class Functor, class Apply, class Applicative, class Bind, class Monad
    , ($), (>>=), (<$>), (<>), (/=), (||), (<$), (==)
    , bind, const, pure, unit, not, discard
    , Unit
    )
import Data.Maybe (Maybe(..))
import Control.Monad.Reader.Trans (ReaderT, runReaderT, ask, lift)
import Effect.Aff (Aff)
import Effect.Ref (Ref, read, modify_)
import Data.Either (Either(..))
import Data.Array as A
import Data.Foldable (length)
import Control.Applicative (when)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (setItem)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Parser as AP
import Control.Promise (toAff)
import Effect.Class (liftEffect)

import UI.File as F
import Json (toJson, fromJson)
import Sequent (Sequent, verifyTypes, verifyBindings)
import Symbol
    (Symbol(..), SymbolMap, defaultMap, defaultSymbols, updateMap)
import Proof (Proof, Deduction(..))
import Proof as P
import Deduction (DeductionRule(..))
import UI.Capabilities
    ( Window(..)
    , class ReadSymbols, class WriteSymbols, class ReadSequents
    , class WriteSequents, class ReadProof, class WriteProof, class Error
    , class ReadError, class ReadNav, class Nav, class History, class ReadFile
    , error, errors, canNew
    )

type ProofState =
    { sequents :: Array (Sequent String String String)
    , symbols :: Array Symbol
    , symbolMap :: SymbolMap
    , proof :: Proof
    }

type AppState =
    { history :: Array ProofState
    , future :: Array ProofState
    , present :: ProofState
    , error :: Maybe (Array String)
    , window :: Window
    }

start :: AppState
start =
    { history : []
    , future : []
    , present :
        { sequents : []
        , symbols : defaultSymbols
        , symbolMap : defaultMap
        , proof : P.empty
        }
    , error : Nothing
    , window : NoWindow
    }

startWith :: ProofState -> AppState
startWith present = start { present = present }

startWithErr :: String -> AppState
startWithErr e = start { error = Just [e] }

newtype AppStateM x = AppStateM (ReaderT (Ref AppState) Aff x)

derive newtype instance functorAppStateM :: Functor AppStateM
derive newtype instance applyAppStateM :: Apply AppStateM
derive newtype instance applicativeAppStateM :: Applicative AppStateM
derive newtype instance bindAppStateM :: Bind AppStateM
derive newtype instance monadAppStateM :: Monad AppStateM

get :: AppStateM AppState
get = AppStateM $ do
    ref <- ask
    lift $ liftEffect $ read ref

modify :: (AppState -> AppState) -> AppStateM Unit
modify f = AppStateM $ do
    ref <- ask
    lift $ liftEffect $ modify_ f ref

put :: AppState -> AppStateM Unit
put a = modify (const a)

run :: forall x. Ref AppState -> AppStateM x -> Aff x
run r (AppStateM f) = runReaderT f r

write :: AppStateM Unit
write = do
    state <- get
    let json = toJson
            state.present.symbols state.present.sequents state.present.proof
    let string = stringify json
    AppStateM $ lift $ liftEffect $
        window >>= localStorage >>= setItem "proof" string

instance readSymbolsAppStateM :: ReadSymbols AppStateM where
    getSymbols = _.present.symbols <$> get
    getSymbolMap = _.present.symbolMap <$> get

instance writeSymbolsAppStateM :: WriteSymbols AppStateM where
    addSymbol symbol = get >>= \state ->
        case updateMap state.present.symbolMap symbol of
            Left e -> false <$ error e
            Right newmap -> do
                modify $ _
                    { history = state.history <> [state.present]
                    , future = []
                    , present = state.present
                        { symbols = state.present.symbols <> [symbol]
                        , symbolMap = newmap
                        }
                    , error = Nothing
                    }
                true <$ write

instance readSequentsAppStateM :: ReadSequents AppStateM where
    getSequents = _.present.sequents <$> get

instance writeSequentsAppStateM :: WriteSequents AppStateM where
    addSequent sequent = do
        state <- get
        if verifyTypes sequent then case verifyBindings sequent of
            Nothing -> do
                modify $ _
                    { history = state.history <> [state.present]
                    , future = []
                    , present = state.present
                        { sequents = state.present.sequents <> [sequent] }
                    , error = Nothing
                    }
                true <$ write
            Just e -> false <$ errors e
        else
            false <$ error "Invalid types"

instance readProofAppStateM :: ReadProof AppStateM where
    getProof = _.present.proof <$> get

instance writeProofAppStateM :: WriteProof AppStateM where
    addDeduction deduction@(Deduction d) = get >>= \state ->
        case d.rule of
            Introduction seq i -> case A.index state.present.sequents i of
                Just s | seq == s -> validate state
                _ -> false <$ error "Sequent is not available"
            Definition sym i -> case A.index state.present.symbols i of
                Just (Custom s) | sym == s -> validate state
                _ -> false <$ error "Symbol is not available"
            _ -> validate state
        where
            validate s = case P.addDeduction deduction s.present.proof of
                Left e -> false <$ error e
                Right newproof -> do
                    modify $ _
                        { history = s.history <> [s.present]
                        , future = []
                        , present = s.present
                            { proof = newproof }
                        , error = Nothing
                        }
                    true <$ write

instance errorAppStateM :: Error AppStateM where
    errors e = modify $ _ { error = Just e }
    clear = modify $ _ { error = Nothing }

instance readErrorAppStateM :: ReadError AppStateM where
    getErrors = _.error <$> get

instance readNavAppStateM :: ReadNav AppStateM where
    getWindow = _.window <$> get

instance navAppStateM :: Nav AppStateM where
    setWindow window = modify $ _ { window = window }

instance historyAppStateM :: History AppStateM where
    undo = get >>= \state -> case A.unsnoc state.history of
        Nothing -> pure unit
        Just {init, last} -> do
            modify $ _
                { history = init
                , future = [state.present] <> state.future
                , present = last
                , error = Nothing
                }
            write
    redo = get >>= \state -> case A.uncons state.future of
        Nothing -> pure unit
        Just {head, tail} -> do
            modify $ _
                { history = state.history <> [state.present]
                , future = tail
                , present = head
                , error = Nothing
                }
            write
    new = do
        state <- get
        ableNew <- canNew
        when ableNew $ do
            modify $ _
                { history = state.history <> [state.present]
                , future = []
                , present =
                    { sequents : []
                    , symbols : defaultSymbols
                    , symbolMap : defaultMap
                    , proof : P.empty
                    }
                , error = Nothing
                }
            write
    canUndo = do
        state <- get
        pure $ length state.history /= 0
    canRedo = do
        state <- get
        pure $ length state.future /= 0
    canNew = do
        state <- get
        pure $
            length state.present.sequents /= 0 ||
            length state.present.symbols /= 0 ||
            not (P.isEmpty state.present.proof)

instance readFileAppStateM :: ReadFile AppStateM where
    readFile e = do
        state <- get
        AppStateM $ do
            ref <- ask
            lift $ do
                string <- toAff $ F.readFile e
                liftEffect $ case AP.jsonParser string >>= fromJson of
                    Left err -> modify_
                        (_ { error = Just ["Error loading file: " <> err]})
                        ref
                    Right p -> modify_
                        ( _
                            { history = state.history <> [state.present]
                            , future = []
                            , present = p
                            , error = Nothing
                            }
                        )
                        ref
        write
