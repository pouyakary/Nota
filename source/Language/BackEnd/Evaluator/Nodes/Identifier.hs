
module Language.BackEnd.Evaluator.Nodes.Identifier ( evalIdentifier ) where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Time
import           Language.BackEnd.Evaluator.Types
import           Language.FrontEnd.AST
import           Model
import           System.IO.Unsafe

-- ─── EVAL IDENTIFIER ────────────────────────────────────────────────────────────

evalIdentifier :: LeafEvalSignature
evalIdentifier ( ASTIdentifier name ) scopePrototype =
    case lookupInConstants name of
        Just x  -> Right x
        Nothing -> lookupInScopePrototype name scopePrototype

-- ─── SCOPE PROTOTYPE ────────────────────────────────────────────────────────────

lookupInScopePrototype :: String -> ScopePrototype -> EvalResult
lookupInScopePrototype name scopePrototype =
    case Map.lookup name scopePrototype of
        Just x  -> Right x
        Nothing -> Left $ "Variable \"" ++ name ++ "\" does not exists."

-- ─── CONSTANTS ──────────────────────────────────────────────────────────────────

lookupInConstants :: String -> Maybe P50
lookupInConstants name =
    case name of
        "π"      -> Just pi :: Maybe P50
        "E"      -> Just ( exp 1 ) :: Maybe P50
        "Karion" -> Just $ unsafePerformIO computeTheKarionTime
        _        -> Nothing

-- Nota's Easter Egg :D
computeTheKarionTime :: IO P50
computeTheKarionTime = do
    birthday <- pure $ pouyaKaryBirthday
    now      <- today
    days     <- pure $ toP50 $ diffDays now birthday
    return days
    where
        pouyaKaryBirthday =
            case fromGregorianValid 1996 1 8 of
                Just day -> day
        today = do
            t <- getCurrentTime
            return $ utctDay t
        toP50 x =
            read ( show x ) :: P50

-- ────────────────────────────────────────────────────────────────────────────────
