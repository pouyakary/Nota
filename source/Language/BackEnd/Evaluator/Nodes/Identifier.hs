
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
evalIdentifier ( ASTIdentifier name ) model =
    case lookupInConstants name of
        Just x  -> Right x
        Nothing -> lookupInScopePrototype name model

-- ─── SCOPE PROTOTYPE ────────────────────────────────────────────────────────────

lookupInScopePrototype :: String -> Model -> EvalResult
lookupInScopePrototype name model =
    case Map.lookup name $ prototype model of
        Just x  -> Right x
        Nothing -> Left $ "Variable \"" ++ name ++ "\" does not exists."

-- ─── CONSTANTS ──────────────────────────────────────────────────────────────────

lookupInConstants :: String -> Maybe Double
lookupInConstants name =
    case name of
        "π"      -> Just pi :: Maybe Double
        "e"      -> Just ( exp 1 ) :: Maybe Double
        -- "Karion" -> Just $ unsafePerformIO computeTheKarionTime
        _        -> Nothing

-- -- Nota's Easter Egg :D
-- computeTheKarionTime :: IO Double
-- computeTheKarionTime = do
--     birthday <- pure $ pouyaKaryBirthday
--     now      <- today
--     days     <- pure $ diffDays now birthday
--     return (read (show days) :: Double)
--     where
--         pouyaKaryBirthday =
--             case fromGregorianValid 1996 1 8 of
--                 Just day -> day
--         today = do
--             t <- getCurrentTime
--             return $ utctDay t

-- ────────────────────────────────────────────────────────────────────────────────
