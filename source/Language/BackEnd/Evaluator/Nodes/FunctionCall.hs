
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
evalFunctionCall ( evalFunc ) ( ASTFunctionCall ( ASTIdentifier name ) args ) model =
    case name of
        "Out" ->
            execOutFunc evalFunc args model

        "Sqrt" ->
            runSingleArgFunc "Square Bracket" sqrt
        "Root" ->
            computeNthRoot evalFunc args model
        "Log" ->
            computeLogarithm evalFunc args model

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
            runSingleArgumentedFunction model evalFunc args
        runArrayArgFunc =
            runFunctionOnArray evalFunc args model

-- ─── SIGN ───────────────────────────────────────────────────────────────────────

sgnFunc :: Double -> Double
sgnFunc x =
    if x == 0 then 0 else ( if x > 0 then 1 else -1 )

-- ─── LOGARITHM ──────────────────────────────────────────────────────────────────

computeLogarithm (evalFunc) arguments model =
    case length arguments of
        2 ->
            case evalFunc (arguments !! 0) model of
                Left xError ->
                    Left xError
                Right baseResult ->
                    case evalFunc (arguments !! 1) model of
                        Left baseError ->
                            Left baseError
                        Right xResult ->
                            Right $ logBase baseResult xResult
        1 ->
            case evalFunc (arguments !! 0) model of
                Left error ->
                    Left error
                Right result ->
                    Right $ log result
        _ ->
            Left $ functionGetsThisMuchArguments "Logarithm" "one or two"

-- ─── ROOT ───────────────────────────────────────────────────────────────────────

computeNthRoot (evalFunc) arguments model =
    case length arguments of
        2 ->
            case evalFunc (arguments !! 0) model of
                Left xError ->
                    Left xError
                Right baseResult ->
                    case evalFunc (arguments !! 1) model of
                        Left baseError ->
                            Left baseError
                        Right xResult ->
                            Right $ xResult ** (1 / baseResult)
        _ ->
            Left $ functionGetsThisMuchArguments "Nth Root" "two"

-- ─── OUT PUT FUNCTION ───────────────────────────────────────────────────────────

execOutFunc (evalFunc) arguments model =
    case length arguments of
        1 ->
            case evalFunc ( arguments !! 0 ) model of
                Left error ->
                    Left error
                Right historyNumber ->
                    if index > 0 && index <= ( length historyOfResults )
                        then Right $ historyOfResults !! ( index - 1 )
                        else Left $ "Output no. " ++ ( show index ) ++ " does not exists."
                    where
                        index =
                            floor historyNumber
        _ ->
            Left $ functionGetsThisMuchArguments "Out" "one"
    where
        historyOfResults =
            computedHistory model

-- ─── RUN FUNCTION ON ARRAY ──────────────────────────────────────────────────────

runFunctionOnArray (evalFunc) arguments model name (computeFunc) =
    case length arguments of
        0 ->
            Left $ functionGetsThisMuchArguments name "at least one"
        1 ->
            case evalFunc (arguments !! 0) model of
                Left error ->
                    Left error
                Right result ->
                    Right result
        _ ->
            case evalFunc (arguments !! 0) model of
                Left error ->
                    Left error
                Right result ->
                    case runFunctionOnArray evalFunc (tail arguments) model name computeFunc of
                        Left restError ->
                            Left restError
                        Right restResult ->
                            Right $ computeFunc [ result, restResult ]

-- ─── RUN SINGLE ARGUMENT FUNCTION ───────────────────────────────────────────────

runSingleArgumentedFunction model (evalFunc) arguments name (computeFunc) =
    case length arguments of
        1 ->
            case evalFunc (arguments !! 0) model of
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