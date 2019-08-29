
module Language.BackEnd.Evaluator.Nodes.FunctionCall ( evalFunctionCall ) where

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
        "Ceil" ->
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
            runSingleArgFunc "Area Sine" asin
        "Acos" ->
            runSingleArgFunc "Area Cosine" acos
        "Atan" ->
            runSingleArgFunc "Area Tangent" atan

        "Sinh" ->
            runSingleArgFunc "Hyperbolic Sine" sinh
        "Cosh" ->
            runSingleArgFunc "Hyperbolic Cosine" cosh
        "Sech" ->
            runSingleArgFunc "Hyperbolic Secant" (\x -> (1 / (sinh x)))
        "Csch" ->
            runSingleArgFunc "Hyperbolic Cosecant" (\x -> (1 / (cosh x)))
        "Tanh" ->
            runSingleArgFunc "Hyperbolic Tangent" tanh
        "Asinh" ->
            runSingleArgFunc "Hyperbolic Area Sine" asinh
        "Acosh" ->
            runSingleArgFunc "Hyperbolic Area Cosine" acosh
        "Atanh" ->
            runSingleArgFunc "Hyperbolic Area Tangent" atanh

        "Max" ->
            runArrayArgFunc "Maximum" maximum
        "Min" ->
            runArrayArgFunc "Minimum" minimum
        "Sum" ->
            runArrayArgFunc "Sum" sum

        "Exp" ->
            runSingleArgFunc "Exponent" exp

        "Sgn" ->
            runSingleArgFunc "Sign" sgnFunc

        _ ->
            Left $ "Function \"" ++ name ++ "\" does not exist."

    where
        runSingleArgFunc =
            runSingleArgumentedFunction scopePrototype evalFunc args
        runArrayArgFunc =
            runFunctionOnArray evalFunc args scopePrototype

-- ─── SIGN ───────────────────────────────────────────────────────────────────────

sgnFunc :: Double -> Double
sgnFunc x =
    if x == 0 then 0 else ( if x > 0 then 1 else -1 )

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

-- ─── RUN FUNCTION ON ARRAY ──────────────────────────────────────────────────────

runFunctionOnArray (evalFunc) arguments scopePrototype name (computeFunc) =
    case length arguments of
        0 ->
            Left $ functionGetsThisMuchArguments name "at least one"
        1 ->
            case evalFunc (arguments !! 0) scopePrototype of
                Left error ->
                    Left error
                Right result ->
                    Right result
        _ ->
            case evalFunc (arguments !! 0) scopePrototype of
                Left error ->
                    Left error
                Right result ->
                    case runFunctionOnArray evalFunc (tail arguments) scopePrototype name computeFunc of
                        Left restError ->
                            Left restError
                        Right restResult ->
                            Right $ computeFunc [ result, restResult ]

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
    "Function '" ++ name ++ "' accepts only " ++ size ++ " argument."

-- ────────────────────────────────────────────────────────────────────────────────