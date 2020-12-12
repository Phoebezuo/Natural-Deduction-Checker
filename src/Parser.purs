module Parser
    ( parseSymbol
    , parse
    , parseMany
    ) where

import Prelude
    (($), (||), (&&), (<$>), (<>), (<<<), (>>>), (<*), (<$), bind, pure, show)
import Control.Alt ((<|>))
import Control.Lazy (class Lazy, defer)
import Data.Array as A
import Data.Char.Unicode as U
import Data.Either (Either(..))
import Data.Either as E
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators ((<?>))
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS
import Text.Parsing.Parser.Token as PT
import Text.Parsing.Parser.Pos as PP
import Data.String as S

import WFF (BinaryOp, WFF, Quantifier)
import WFF as WFF
import Symbol (SymbolMap, Operator(..))

type StringWFFParser = Parser String (WFF String String String)
type StringWFFQuantParser = Parser String (Either
    { operator :: Quantifier, variable :: String }
    (WFF String String String)
    )

symbol :: Parser String String
symbol = fromCharArray <$> A.some
    ( PS.satisfy
        ( (U.isPunctuation || U.isSymbol) &&
        (_ `A.notElem` [',','(',')']) )
        <?> "Symbol or Punctuation"
    )

definedSymbol :: SymbolMap -> Parser String Operator
definedSymbol m = do
    p <- P.position
    s <- symbol
    case M.lookup s m of
        Just o -> pure o
        Nothing -> P.failWithPosition ("Unrecognised symbol: " <> s) p

variables :: Parser String (Array String)
variables = PC.chainl1
    (pure <<< fromCharArray <$> A.some PT.letter)
    ((<>) <$ PS.char ',')

predicate :: StringWFFParser
predicate = do
    name <- fromCharArray <$> A.some PT.letter
    vars <- PC.option [] $ PC.between  (PS.char '(') (PS.char ')') variables
    pure $ WFF.Pred { predicate : name, variables : WFF.Free <$> vars }

bracketedOrQuantified :: Lazy StringWFFParser => SymbolMap -> StringWFFParser
bracketedOrQuantified m = do
    bracketed <- PC.between (PS.char '(') (PS.char ')')
        (defer \_ -> expressionOrQ m)
    case bracketed of
        Left { operator : WFF.Forall, variable } ->
            defer \_ -> WFF.foralv variable <$> safeExpression m
        Left { operator : WFF.Exists, variable } ->
            defer \_ -> WFF.exists variable <$> safeExpression m
        Right w -> pure w

safeExpression :: Lazy StringWFFParser => SymbolMap -> StringWFFParser
safeExpression m = predicate <|> bracketedOrQuantified m

startSymbolExpression :: Lazy StringWFFQuantParser =>
    SymbolMap -> StringWFFQuantParser
startSymbolExpression m = do
    symbolPos <- P.position
    o <- definedSymbol m
    case o of
        NullaryOperator operator ->
            pure $ Right $ WFF.Nullary operator
        UnaryOperator operator -> do
            contents <- safeExpression m
            pure $ Right $ WFF.Unary { operator, contents }
        QuantOperator operator -> do
            variable <- fromCharArray <$> A.some PT.letter
            pure $ Left $ { operator, variable }
        _ -> P.failWithPosition
            "Expected Nullary, Unary, or Quantifier Symbol" symbolPos

tailBinaryExpression :: Lazy StringWFFParser => SymbolMap ->
    Parser String { operator :: BinaryOp, right :: WFF String String String }
tailBinaryExpression m = do
    p <- P.position
    o <- definedSymbol m
    right <- safeExpression m
    case o of
        BinaryOperator operator -> pure { operator, right }
        _ -> P.failWithPosition "Expected Binary Symbol" p

maybeBinaryExpression :: Lazy StringWFFParser => SymbolMap -> StringWFFParser
maybeBinaryExpression m = do
    left <- safeExpression m
    rest <- PC.optionMaybe $ tailBinaryExpression m
    case rest of
        Nothing -> pure left
        Just {right, operator} -> pure $ WFF.Binary $ {left, operator, right}

expressionOrQ :: Lazy StringWFFQuantParser => SymbolMap -> StringWFFQuantParser
expressionOrQ m =
    (Right <$> maybeBinaryExpression m) <|> startSymbolExpression m

expression :: Lazy StringWFFParser => SymbolMap -> StringWFFParser
expression m = do
    p <- P.position
    x <- expressionOrQ m
    case x of
        Left _ -> P.failWithPosition "Unexpected quantifier" p
        Right y -> pure y

manyExpression :: Lazy (Parser String (Array (WFF String String String))) =>
    SymbolMap -> Parser String (Array (WFF String String String))
manyExpression m = PC.chainl
    (A.singleton <$> expression m)
    ((<>) <$ PS.char ',')
    []

showError :: String -> P.ParseError -> String
showError s (P.ParseError e (PP.Position p)) =
    "Parsing Error: " <>
    e <>
    " at position " <>
    show p.column <>
    " when parsing \"" <>
    s <>
    "\""

removeSpaces :: String -> String
removeSpaces = S.replaceAll (S.Pattern " ") (S.Replacement "")

parseSymbol :: String -> Either String String
parseSymbol s = E.either (showError s >>> Left) Right $ P.runParser s $
    symbol <* PS.eof

parse :: SymbolMap -> String -> Either String (WFF String String String)
parse m s = E.either (showError (removeSpaces s) >>> Left) Right $
    P.runParser (removeSpaces s) $
    expression m <* PS.eof

parseMany :: SymbolMap -> String ->
    Either String (Array (WFF String String String))
parseMany m s = E.either (showError (removeSpaces s) >>> Left) Right $
    P.runParser (removeSpaces s) $
    manyExpression m <* PS.eof
