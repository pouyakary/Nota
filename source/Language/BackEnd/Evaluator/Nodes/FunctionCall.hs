
module Language.BackEnd.Evaluator.Nodes.FunctionCall where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Time
import           Language.BackEnd.Evaluator.Types
import           Language.FrontEnd.AST
import           Model
import           System.IO.Unsafe

-- ─── EVAL FUNCTION CALL ─────────────────────────────────────────────────────────

evalFunctionCall :: StemEvalSignature
evalFunctionCall ( evalFunc ) ( ASTFunctionCall (ASTIdentifier name) args ) scopePrototype =
    case name of
        "Sqrt" ->
            runSingleArgFunc "Square Bracket" sqrt
        "Log" ->
            computeLogarithm evalFunc args scopePrototype

        "Abs" ->
            runSingleArgFunc "Absolute" abs

        "Floor" ->
            runSingleArgFunc "Floor" (\x -> (read (show (floor x)) :: Double))
        "Ceiling" ->
            runSingleArgFunc "Ceiling" (\x -> (read (show (ceiling x)) :: Double))

        "Sin" ->
            runSingleArgFunc "Sine" sin
        "Cos" ->
            runSingleArgFunc "Cosine" cos
        "Tan" ->
            runSingleArgFunc "Tangent" tan
        "Cot" ->
            runSingleArgFunc "Cotangent" (\x -> ((cos x) / (sin x)))
        "Sec" ->
            runSingleArgFunc "Secant" (\x -> (1 / (cos x)))
        "Csc" ->
            runSingleArgFunc "Cosecant" (\x -> (1 / (sin x)))

        "Asin" ->
            runSingleArgFunc "Arc Sine" asin
        "Acos" ->
            runSingleArgFunc "Arc Cosine" acos
        "Atan" ->
            runSingleArgFunc "Arc Tangent" atan

        _ ->
            Left $ "Function \"" ++ name ++ "\" Does not exist."

    where
        runSingleArgFunc =
            runSingleArgumentedFunction scopePrototype evalFunc args

-- ─── LOGARITHM ──────────────────────────────────────────────────────────────────

computeLogarithm (evalFunc) arguments scopePrototype =
    case length arguments of
        2 ->
            case evalFunc (arguments !! 0) scopePrototype of
                Left xError ->
                    Left xError
                Right baseResult ->
                    case evalFunc (arguments !! 1) scopePrototype of
                        Left baseError ->
                            Left baseError
                        Right xResult ->
                            Right $ logBase baseResult xResult
        1 ->
            case evalFunc (arguments !! 0) scopePrototype of
                Left error ->
                    Left error
                Right result ->
                    Right $ log result
        _ ->
            Left $ functionGetsThisMuchArguments "Logarithm" "one or two"


-- ─── RUN SINGLE ARGUMENT FUNCTION ───────────────────────────────────────────────

runSingleArgumentedFunction scopePrototype (evalFunc) arguments name (computeFunc) =
    case length arguments of
        1 ->
            case evalFunc (arguments !! 0) scopePrototype of
                Left error ->
                    Left error
                Right result ->
                    Right $ computeFunc result
        _ ->
            Left $ functionGetsThisMuchArguments name "one"

-- ─── FUNCTION GETS THIS MUCH ARGUMNETS ──────────────────────────────────────────

functionGetsThisMuchArguments name size =
    name ++ " Function takes accepts only " ++ size ++ " argument."

-- ────────────────────────────────────────────────────────────────────────────────