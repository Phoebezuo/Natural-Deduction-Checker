module UI.Capabilities
    ( Window(..)
    , class ReadSymbols
    , getSymbols
    , getSymbolMap
    , parse
    , parseMany
    , class WriteSymbols
    , addSymbol
    , class ReadSequents
    , getSequents
    , class WriteSequents
    , addSequent
    , class ReadProof
    , getProof
    , class WriteProof
    , addDeduction
    , class Error
    , errors
    , clear
    , error
    , class ReadError
    , getErrors
    , isError
    , class ReadNav
    , getWindow
    , isSymbolWindow
    , isSequentWindow
    , isIEWindow
    , class Nav
    , setWindow
    , close
    , class History
    , undo
    , redo
    , new
    , canUndo
    , canRedo
    , canNew
    , class ReadFile
    , readFile
    ) where

import Prelude
    ( class Monad, class Eq
    , Unit
    , pure, unit
    , (<<<), (==), (<$>), (/=), (*>), (>>=), ($), (<*)
    )
import Data.Maybe (Maybe(Nothing))
import Data.Either (Either)
import Halogen (HalogenM, lift, raise)
import Web.Event.Event (Event)

import Symbol (Symbol, SymbolMap)
import Sequent (Sequent)
import Proof (Deduction, Proof)
import WFF (WFF)
import Parser as P

data Window
    = SequentWindow
    | SymbolWindow
    | IEWindow
    | NoWindow

derive instance eqWindow :: Eq Window

class Monad m <= ReadSymbols m where
    getSymbols :: m (Array Symbol)
    getSymbolMap :: m SymbolMap

parse :: forall m. ReadSymbols m => String ->
    m (Either String (WFF String String String))
parse s = getSymbolMap >>= \m -> pure $ P.parse m s

parseMany :: forall m. ReadSymbols m => String ->
    m (Either String (Array (WFF String String String)))
parseMany s = getSymbolMap >>= \m -> pure $ P.parseMany m s

class Monad m <= WriteSymbols m where
    addSymbol :: Symbol -> m Boolean

class Monad m <= ReadSequents m where
    getSequents :: m (Array (Sequent String String String))

class Monad m <= WriteSequents m where
    addSequent :: Sequent String String String -> m Boolean

class Monad m <= ReadProof m where
    getProof :: m Proof

class Monad m <= WriteProof m where
    addDeduction :: Deduction -> m Boolean

class Monad m <= Error m where
    errors :: Array String -> m Unit
    clear :: m Unit

error :: forall m. Error m => String -> m Unit
error = errors <<< pure

class Monad m <= ReadError m where
    getErrors :: m (Maybe (Array String))

isError :: forall m. ReadError m => m Boolean
isError = (_ /= Nothing) <$> getErrors

class Monad m <= ReadNav m where
    getWindow :: m Window

isSymbolWindow :: forall m. ReadNav m => m Boolean
isSymbolWindow = (_ == SymbolWindow) <$> getWindow

isSequentWindow :: forall m. ReadNav m => m Boolean
isSequentWindow = (_ == SequentWindow) <$> getWindow

isIEWindow :: forall m. ReadNav m => m Boolean
isIEWindow = (_ == IEWindow) <$> getWindow

class Monad m <= Nav m where
    setWindow :: Window -> m Unit

close :: forall m. Nav m => m Unit
close = setWindow NoWindow

class Monad m <= History m where
    undo :: m Unit
    redo :: m Unit
    new :: m Unit
    canUndo :: m Boolean
    canRedo :: m Boolean
    canNew :: m Boolean

class Monad m <= ReadFile m where
    readFile :: Event -> m Unit

instance readSymbolsHalogenM :: ReadSymbols m =>
    ReadSymbols (HalogenM s a t o m) where
        getSymbols = lift getSymbols
        getSymbolMap = lift getSymbolMap

instance writeSymbolsHalogenM :: WriteSymbols m =>
    WriteSymbols (HalogenM s a t Unit m) where
        addSymbol s = lift (addSymbol s) <* raise unit

instance readSequentsHalogenM :: ReadSequents m =>
    ReadSequents (HalogenM s a t o m) where
        getSequents = lift getSequents

instance writeSequentsHalogenM :: WriteSequents m =>
    WriteSequents (HalogenM s a t Unit m) where
        addSequent s = lift (addSequent s) <* raise unit

instance readProofHalogenM :: ReadProof m =>
    ReadProof (HalogenM s a t o m) where
        getProof = lift getProof

instance writeProofHalogenM :: WriteProof m =>
    WriteProof (HalogenM s a t Unit m) where
        addDeduction d = lift (addDeduction d) <* raise unit

instance errorHalogenM :: Error m => Error (HalogenM s a t Unit m) where
    errors e = lift (errors e) *> raise unit
    clear = lift clear *> raise unit

instance readErrorHalogenM :: ReadError m =>
    ReadError (HalogenM s a t o m) where
        getErrors = lift getErrors

instance readNavHalogenM :: ReadNav m => ReadNav (HalogenM s a t o m) where
    getWindow = lift getWindow

instance navHalogenM :: Nav m => Nav (HalogenM s a t Unit m) where
    setWindow w = lift (setWindow w) *> raise unit

instance historyHalogenM :: History m => History (HalogenM s a t Unit m) where
    undo = lift undo *> raise unit
    redo = lift redo *> raise unit
    new = lift new *> raise unit
    canUndo = lift canUndo
    canRedo = lift canRedo
    canNew = lift canNew

instance readFileHalogenM :: ReadFile m =>
    ReadFile (HalogenM s a t Unit m) where
        readFile f = lift (readFile f) *> raise unit
