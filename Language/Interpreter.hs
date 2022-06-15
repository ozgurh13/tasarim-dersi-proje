
{-# OPTIONS_GHC  -O2 #-}

{-# LANGUAGE  LambdaCase      #-}
{-# LANGUAGE  BlockArguments  #-}

module Language.Interpreter (runProgram) where

import Language.Types
import qualified Language.Environment as Env

import System.IO (hFlush, stdout)
import System.CPUTime (getCPUTime)

import Control.Monad.State.Strict
import Control.Monad.Except
import qualified Control.Exception as Exception

import Data.Bits (shiftL, shiftR)
import Data.List (genericIndex, genericLength)
import Data.Foldable (traverse_)

import qualified Data.Map.Strict as Map





data InterpreterError
    = UnsupportedOperation String
    | UndefinedVariable String
    | NonBooleanPredicate String
    | WrongNumberOfArguments String
    | WrongTypeOfArguments String
    | CallError String
    | SubscriptError String
    | IndexError String
    | DivisionByZero

    -- these handle flow
    | ReturnException Object
    | BreakException
    deriving (Show)

type InterpreterState = Env.Environment

type Interpreter = ExceptT InterpreterError (StateT InterpreterState IO)


runProgram :: Stmt -> IO (Either InterpreterError ())
runProgram ast = evalStateT (runExceptT (execute ast)) [Env.newEnv]











execute :: Stmt -> Interpreter ()

execute (Block stmt) = modify (Env.newEnv:) >> execute stmt >> modify tail

execute (Seq s) = traverse_ execute s

execute (Expr expr) = evaluate expr >> return ()

execute (FunDecl name params body) = do
    currEnv <- get
    modify \(curr:prev) ->
        Env.define name (Function params body (Env.newEnv:currEnv)) curr : prev

execute (VarDecl name value) = do
    val <- evaluate value
    modify \(curr:prev) -> Env.define name val curr : prev

execute (If cond t f) =
    evaluate cond >>= \case
        Bool b -> execute if b then t else f
        _      -> throwError $ NonBooleanPredicate "expected boolean value in if"

execute (While cond stmt) =
    evaluate cond >>= \case
        Bool b -> when b $ execute (Seq [stmt, While cond stmt])
            `catchError` \case
                BreakException -> return ()
                err            -> throwError err

        _      -> throwError $ NonBooleanPredicate "expected boolean value in loop conditional"

-- try-except should propagate ReturnException and BreakException
execute (TryExcept try except) =
    execute try `catchError` \case
        ReturnException returnValue -> throwError $ ReturnException returnValue
        BreakException              -> throwError BreakException
        _                           -> execute except

execute (Return r) = evaluate r >>= throwError . ReturnException

execute Break = throwError BreakException

execute Pass = return ()

-- all ConstDecls should have already either
--   been propagated
-- or
--   turned into VarDecls
execute (ConstDecl _ _) = errorWithoutStackTrace $ unlines
    [ "error from: Language.Interpreter.execute (ConstDecl _ _)"
    , "This is an internal error and shouldn't have happened"
    , "Please report this as a bug" ]
















evaluate :: Expr -> Interpreter Object

evaluate (Literal l) = case l of
    -- variables evaluate to their value
    Variable v -> gets (Env.get v) >>=
        maybe (throwError $ UndefinedVariable v) return

    -- evaluate all items in the list
    List list  -> mapM (toLiteral . evaluate) list >>= return . List

    -- everything else evaluates to itself
    _          -> return l

    where toLiteral = fmap Literal

evaluate (Unary op expr) = evaluate expr >>= evalUnary op
    where
        evalUnary :: UnOp -> Object -> Interpreter Object
        evalUnary Negate = \case
            n@(Integer _) -> return $ negate n
            n@(Double  _) -> return $ negate n
            _             -> throwError $ UnsupportedOperation "can only negate numbers"

        evalUnary Not = \case
            b@(Bool _) -> return $ negate b
            _          -> throwError $ UnsupportedOperation "can only apply `not` to booleans"

evaluate (Binary op expr1 expr2) = do
    expr1' <- evaluate expr1

    -- handle short circuiting operators first
    if op `elem` [And, Or] then do
        case (op, expr1') of
            (And, Bool b) -> do if b then evaluate expr2 else return (Bool b)
            (Or,  Bool b) -> do if b then return (Bool b) else evaluate expr2
            _             -> throwError $ NonBooleanPredicate "expected boolean value"

    -- operator doesn't require short circuiting
    else do
        expr2' <- evaluate expr2
        case op of
            CmpEQ -> return $ Bool (expr1' == expr2')
            CmpNE -> return $ Bool (expr1' /= expr2')

            CmpLT -> cmpOp (<)  "<"  (expr1', expr2')
            CmpLE -> cmpOp (<=) "<=" (expr1', expr2')
            CmpGT -> cmpOp (>)  ">"  (expr1', expr2')
            CmpGE -> cmpOp (>=) ">=" (expr1', expr2')

            Add -> case expr1' + expr2' of
                Null   -> throwUnsupportedOperation "`+` can only be applied to numbers, strings or lists"
                answer -> return answer
            Sub -> case expr1' - expr2' of
                Null   -> throwUnsupportedOperation "`-` can only be applied to numbers or lists"
                answer -> return answer
            Mul -> case expr1' * expr2' of
                Null   -> throwUnsupportedOperation "`*` can only be applied to numbers or lists"
                answer -> return answer
            Div -> case (expr1', expr2') of
                (_          , Integer 0) -> throwError DivisionByZero
                (_          , Double  0) -> throwError DivisionByZero

                (Integer n1, Integer n2) -> return $ Integer (n1 `div` n2)
                (Integer n1, Double  n2) -> return $ Double (fromIntegral n1 / n2)
                (Double  n1, Integer n2) -> return $ Double (n1 / fromIntegral n2)
                (Double  n1, Double  n2) -> return $ Double (n1 / n2)
                _                        -> throwUnsupportedOperation "`/` can only be applied to numbers"

            Mod -> case (expr1', expr2') of
                (_         , Integer 0)  -> throwError DivisionByZero
                (Integer n1, Integer n2) -> return $ Integer (n1 `mod` n2)
                _                        -> throwUnsupportedOperation "`%` can only be applied to integers"
            Pow -> case (expr1', expr2') of
                (Integer n1, Integer n2) -> return $ Integer (n1 ^ n2)
                (Integer n1, Double  n2) -> return $ Double (fromIntegral n1 ** n2)
                (Double  n1, Integer n2) -> return $ Double (n1 ** fromIntegral n2)
                (Double  n1, Double  n2) -> return $ Double (n1 ** n2)
                _                        -> throwUnsupportedOperation "`**` can only be applied to numbers"

            ShiftL -> case (expr1', expr2') of
                (Integer n1, Integer n2) -> return $ Integer (n1 `shiftL` fromIntegral n2)
                _                        -> throwUnsupportedOperation "`<<` can only be applied to integers"
            ShiftR -> case (expr1', expr2') of
                (Integer n1, Integer n2) -> return $ Integer (n1 `shiftR` fromIntegral n2)
                _                        -> throwUnsupportedOperation "`>>` can only be applied to integers"

            _                            -> error "interpreter evaluate"

    where
        throwUnsupportedOperation = throwError . UnsupportedOperation

        cmpOp cmp _ (a@(Integer _), b@(Integer _)) = return $ Bool (a `cmp` b)
        cmpOp cmp _ (a@(Integer _), b@(Double _))  = return $ Bool (a `cmp` b)
        cmpOp cmp _ (a@(Double _),  b@(Integer _)) = return $ Bool (a `cmp` b)
        cmpOp cmp _ (a@(Double _),  b@(Double _))  = return $ Bool (a `cmp` b)
        cmpOp _  opStr _ = throwUnsupportedOperation $
                      "`" ++ opStr ++ "` can only be applied to numbers"


evaluate (Assign name expr) = do
    isDefined <- gets (Env.defined name)
    if isDefined then do
        value <- evaluate expr
        modify $ Env.assign name value
        return value       -- assignment returns the value
    else throwError $ UndefinedVariable name


evaluate (Lambda args body) = do
    env <- get
    return $ Function args (Block (Return body)) (Env.newEnv:env)


evaluate (Call fn args) = do
    args' <- mapM evaluate args
    let lengthArgs = length args
    case fn of
        "print" -> liftIO (builtinPrint args') >> return Null

        "input" ->
            if lengthArgs > 1 then throwError . WrongNumberOfArguments $
                "input takes at most 1 argument but was given " ++ show lengthArgs
            else case args' of
                    [String s] -> liftIO $ builtinInput s
                    []         -> liftIO $ builtinInput ""
                    _          -> throwError $ WrongTypeOfArguments
                                    "input only takes in strings as arguments"



        "length" ->
            if lengthArgs /= 1 then throwError $ WrongNumberOfArguments $
                "length takes in 1 argument but was given " ++ show lengthArgs
            else case args' of
                    [List list] -> return $ Integer (genericLength list)
                    [String s]  -> return $ Integer (genericLength s)
                    _           -> throwError $ WrongTypeOfArguments
                                    "length only takes in lists or strings as arguments"


        "cons" ->
            if lengthArgs /= 2 then throwError $ WrongNumberOfArguments $
                "cons takes 2 arguments but was given " ++ show lengthArgs
            else case args' of
                    [l, List list] -> return $ List (Literal l : list)
                    _              -> throwError . WrongTypeOfArguments $
                                        "second argument to cons must be a list"
        "uncons" ->
            if lengthArgs /= 1 then throwError . WrongNumberOfArguments $
                "uncons takes 1 argument but was given " ++ show lengthArgs
            else case args' of
                    [List    [] ] -> return Null
                    [List (x:xs)] -> return $ List [x, Literal (List xs)]
                    _             -> throwError $ WrongTypeOfArguments
                                        "uncons only takes list as an arguments"



        "assert" -> Exception.assert (all (== (Bool True)) args') (return Null)

        "abort" -> errorWithoutStackTrace "aborted"

        "read" ->
            if lengthArgs > 1 then throwError $ WrongNumberOfArguments $
                    "read takes 1 argument but was given " ++ show lengthArgs
            else case args' of
                    [String filename] -> liftIO $ builtinRead filename
                    _                 -> throwError $ WrongTypeOfArguments
                                            "read only takes in a string as an arguments"

        "time" -> liftIO getCPUTime >>= return . Integer


        userDefinedFunc -> do
            function <- gets (Env.get userDefinedFunc)
            let function' = maybe Null id function
            case function' of
                f@(Function params (Block body) (env:rest)) -> do
                    let lenP = length params
                        lenA = length args'
                    if lenA > lenP then throwError $ WrongNumberOfArguments $
                            userDefinedFunc ++ " takes " ++ show lenP
                            ++ " argument(s) but was given " ++ show lenA
                    else do
                        let (x, y) = splitAt lenA params
                            newEnv = Map.fromList (zip x args') `Map.union` env
                        if null y then do
                            -- all parameters have been passed in
                            -- proceed to call the function
                            --
                            -- get the current environment as it will be restored
                            -- execute the function in the environment it was defined in
                            -- executing the function may alter its environment
                            --   so we capture the changes and redefine the function
                            --   with the new environment
                            oldEnv <- get

                            -- define the function itself so we can have recursive functions
                            -- TODO:
                            --     consider emptying f's env before defining
                            --     this way recursive calls won't be affected by partially applied functions
                            put (Map.fromList [(userDefinedFunc, f)]:newEnv:rest)

                            returnValue <- do { execute body
                                              ; return Null
                                              } `catchError` \case
                                    ReturnException r -> return r
                                    err               -> throwError err

                            -- throw away the function's own environment
                            --   as it might be different with each call
                            --   (different arguments may be passed in resulting
                            --   in different values being produced inside)
                            -- restore old environment
                            -- define function again with new environment
                            thisEnv <- gets tail
                            put oldEnv
                            modify $ Env.assign userDefinedFunc
                                (Function params (Block body) thisEnv)

                            return returnValue

                        else    -- not enough arguments, return a function that awaits the rest
                            return (Function y (Block body) (newEnv:rest))

                _                  -> throwError . CallError $ "cannot call " ++ userDefinedFunc

    where
        -- builtin functions
        builtinPrint = putStrLn . unwords . map show

        builtinInput str = putStr str >> hFlush stdout >> getLine >>= return . String

        builtinRead file = readFile file >>= return . String



evaluate (Subscript var index) = do
    i <- evaluate index >>= \case
        Integer i -> return i
        _         -> throwError $ SubscriptError "subscript must be an integer"

    var' <- gets (Env.get var)
    case maybe Null id var' of
        List list     -> if i >= genericLength list then
                            throwError $ formatIndexError "list" i (length list)
                         else evaluate $ list `genericIndex` i

        String string -> if i >= genericLength string then
                            throwError $ formatIndexError "string" i (length string)
                         else return $ String [string `genericIndex` i]

        _             -> throwError $ SubscriptError $ var ++ " isn't subscriptable"

    where formatIndexError name i len = IndexError $
            "cannot index " ++ show i ++ " in a " ++ name ++ " of length " ++ show len



evaluate (Ternary cond texpr fexpr) =
    evaluate cond >>= \case
        Bool b -> evaluate $ if b then texpr else fexpr
        _      -> throwError $ NonBooleanPredicate "expected boolean in ternary expression"

