
{-# OPTIONS_GHC  -O2 #-}

{-# LANGUAGE  LambdaCase      #-}
{-# LANGUAGE  BlockArguments  #-}


module Language.Optimizer (simplify, optimize) where

{-
 | Optimizer
 |    transform the given ast into
 |    a more efficient ast
 |
 |
 |  optimizations
 |    -> simplification
 |          simplifies expressions to normal form
 |
 |    -> constant propagation
 |          propagates the values of constants so
 |          they won't need to be looked up during
 |          runtime
 |
 -}


import Language.Types

import Control.Arrow (first, second)
import Control.Monad.State.Strict

import Data.Bits (shiftL, shiftR)

import qualified Data.Set        as Set
import qualified Data.Map.Strict as Map


optimize :: Stmt -> Stmt
optimize = propagateConst





-- simplifies a statement
simplify :: Stmt -> Stmt

-- if Seq only has a single statement, replace Seq with statement
simplify (Seq [])     = Pass
simplify (Seq [stmt]) = simplify stmt
simplify (Seq stmts)  = Seq $ filter (/= Pass) (map simplify stmts)

simplify (Block block) =
    case simplify block of
        Pass  -> Pass
        block -> Block block

-- if the condition can be evaluated before runtime
--   replace the if with the corresponding branch
simplify (If b ts fs) =
    let ts' = simplify ts
        fs' = simplify fs
     in case reduceToNormalForm b of
        (Literal (Bool b)) -> if b then ts' else fs'
        cond               -> If cond ts' fs'

simplify (While b s) =
    case reduceToNormalForm b of
        (Literal (Bool False)) -> Pass
        cond                   -> While cond (simplify s)

simplify (Expr expr) = Expr (reduceToNormalForm expr)

simplify (VarDecl s e) = VarDecl s (reduceToNormalForm e)

simplify (ConstDecl s e) = ConstDecl s (reduceToNormalForm e)

simplify (FunDecl s p b) = FunDecl s p (simplify b)

simplify (TryExcept t e) = TryExcept (simplify t) (simplify e)

simplify (Return e) = Return (reduceToNormalForm e)

simplify Break = Break

simplify Pass = Pass








-- reduces expressions to normal form
--    2 + 1           ==>   3
--    true && false   ==>   false
reduceToNormalForm :: Expr -> Expr

reduceToNormalForm l@(Literal lit) =
    case lit of
        List list -> Literal $ List (fmap reduceToNormalForm list)
        _         -> l

reduceToNormalForm (Unary op expr) =
    let expr' = reduceToNormalForm expr
     in case op of
          Negate -> case expr' of
              Literal (Integer i) -> Literal (Integer (-i))
              Literal (Double d)  -> Literal (Double (-d))
              _                   -> Unary Negate expr'
          Not    -> case expr' of
              Literal (Bool b) -> Literal (Bool (not b))
              _                -> Unary Not expr'

reduceToNormalForm (Binary op expr1 expr2) =
    let expr1' = reduceToNormalForm expr1
        expr2' = reduceToNormalForm expr2
        exprs  = (expr1', expr2')
     in case op of
          And -> case expr1' of
                    f@(Literal (Bool False)) -> f
                    Literal (Bool True)      -> expr2'
                    _                        -> Binary And expr1' expr2'
          Or  -> case expr1' of
                    t@(Literal (Bool True)) -> t
                    Literal (Bool False)    -> expr2'
                    _                       -> Binary Or expr1' expr2'

          CmpLT -> cmpOp CmpLT (<)  exprs
          CmpLE -> cmpOp CmpLE (<=) exprs
          CmpGT -> cmpOp CmpGT (>)  exprs
          CmpGE -> cmpOp CmpGE (>=) exprs

          CmpEQ -> cmpEq CmpEQ (==) exprs
          CmpNE -> cmpEq CmpNE (/=) exprs

          Add -> arithOp Add (+) exprs
          Sub -> arithOp Sub (-) exprs
          Mul -> arithOp Mul (*) exprs
          Div -> case exprs of
                  -- if there is a division by zero, leave it to the runtime to throw the error
                    (_,                    Literal (Integer  0)) -> Binary Div expr1' expr2'
                    (_,                    Literal (Double   0)) -> Binary Div expr1' expr2'

                    (Literal (Integer i1), Literal (Integer i2)) -> Literal $ Integer (i1 `div` i2)
                    (Literal (Integer i1), Literal (Double d2))  -> Literal $ Double (fromIntegral i1 / d2)
                    (Literal (Double d1),  Literal (Integer i2)) -> Literal $ Double (d1 / fromIntegral i2)
                    (Literal (Double d1),  Literal (Double d2))  -> Literal $ Double (d1 / d2)

                    (left,                 right)                -> Binary Div left right
          Mod -> case exprs of
                  -- if there is a modulus by zero, leave it to the runtime to throw the error
                    (_,                    Literal (Integer  0)) -> Binary Mod expr1' expr2'
                    (Literal (Integer i1), Literal (Integer i2)) -> Literal $ Integer (i1 `mod` i2)
                    (left,                 right)                -> Binary Mod left right
          Pow -> case exprs of
                    (Literal (Integer i1), Literal (Integer i2)) -> Literal $ Integer (i1 ^ i2)
                    (Literal (Integer i1), Literal (Double d2))  -> Literal $ Double (fromIntegral i1 ** d2)
                    (Literal (Double d1),  Literal (Integer i2)) -> Literal $ Double (d1 ** fromIntegral i2)
                    (Literal (Double d1),  Literal (Double d2))  -> Literal $ Double (d1 ** d2)
                    (left,                 right)                -> Binary Pow left right

          ShiftL -> case exprs of
                    (Literal (Integer i1), Literal (Integer i2)) -> Literal $ Integer (i1 `shiftL` fromIntegral i2)
                    (left,                 right)                -> Binary ShiftL left right
          ShiftR -> case exprs of
                    (Literal (Integer i1), Literal (Integer i2)) -> Literal $ Integer (i1 `shiftR` fromIntegral i2)
                    (left,                 right)                -> Binary ShiftR left right

    where
        cmpOp constructor op = \case
            (Literal a@(Integer _), Literal b@(Integer _)) -> Literal $ Bool (a `op` b)
            (Literal a@(Integer _), Literal b@(Double _))  -> Literal $ Bool (a `op` b)
            (Literal a@(Double _),  Literal b@(Integer _)) -> Literal $ Bool (a `op` b)
            (Literal a@(Double _),  Literal b@(Double _))  -> Literal $ Bool (a `op` b)
            (left,                  right)                 -> Binary constructor left right

        cmpEq constructor op = \case
            (a@(Literal (Variable _)), b) -> Binary constructor a b
            (a, b@(Literal (Variable _))) -> Binary constructor a b
            (Literal a, Literal b)        -> Literal $ Bool (a `op` b)
            (left,      right)            -> Binary constructor left right

        arithOp constructor op e@(x, y) = case e of
            (Literal a, Literal b) -> case a `op` b of
                                        Null -> Binary constructor x y
                                        res  -> Literal res
            (left,      right)     -> Binary constructor left right


reduceToNormalForm (Call s exprs) = Call s (map reduceToNormalForm exprs)

reduceToNormalForm (Assign s expr) = Assign s (reduceToNormalForm expr)

reduceToNormalForm (Lambda args body) = Lambda args (reduceToNormalForm body)

reduceToNormalForm (Ternary e1 e2 e3) =
    let texpr = reduceToNormalForm e2
        fexpr = reduceToNormalForm e3
     in case reduceToNormalForm e1 of
        Literal (Bool b) -> if b then texpr else fexpr
        cond             -> Ternary cond texpr fexpr

reduceToNormalForm (Subscript s e) = Subscript s (reduceToNormalForm e)




























--
-- propagate values of constant values
--
-- when this function is called, it means the program
--   has passed the validation check, so we can use a
--   single map as we won't need to keep track of scopes
--
--   we know there can't be conflicting definitions of
--   variables
--
-- we use a set to keep track of defined variables in scope
--   so we don't accidentally overwrite a variable defined
--   in the current scope [see: Issue 001]
--
--   we do however need to keep track of scope for variables
--
propagateConst :: Stmt -> Stmt
propagateConst = simplify . flip evalState (Set.empty, Map.empty) . propConstStmt




--                             variables          constants
type ConstPropagatorState = (Set.Set String, Map.Map String Expr)



propConstStmt :: Stmt -> State ConstPropagatorState Stmt
propConstStmt (Seq s)   = Seq <$> traverse propConstStmt s
-- blocks introduce a new scope
propConstStmt (Block b) = do
    currNames <- gets fst
    modify (first (const Set.empty))
    b' <- propConstStmt b
    modify (first (const currNames))
    return $ Block b'
propConstStmt (If expr ts fs)
    = If <$> propConstExpr expr <*> propConstStmt ts <*> propConstStmt fs
propConstStmt (While expr stmt)
    = While <$> propConstExpr expr <*> propConstStmt stmt
propConstStmt (Expr expr) = Expr <$> propConstExpr expr

-- if the value of const is known propagate it, and remove the const decleration
-- if the value is unknown, propagate the const values through the rhs
--     and change it to be a variable decleration
-- don't propagate lists and strings as they are subscriptable
--     and they are looked up by their name
propConstStmt (ConstDecl str val) = case val of
        Literal (List   _) -> VarDecl str <$> propConstExpr val
        Literal (String _) -> VarDecl str <$> propConstExpr val
        Literal         _  -> modify (second (Map.insert str val)) >> return Pass
        _                  -> VarDecl str <$> propConstExpr val

-- add variable name to set
propConstStmt (VarDecl var val) = do
    modify (first (Set.insert var))
    val' <- propConstExpr val
    return $ VarDecl var val'
propConstStmt (FunDecl f a b) = FunDecl f a <$> propConstStmt b
propConstStmt (TryExcept t e) = TryExcept <$> propConstStmt t <*> propConstStmt e
propConstStmt (Return r)      = Return <$> propConstExpr r

propConstStmt Break = return Break
propConstStmt Pass  = return Pass









propConstExpr :: Expr -> State ConstPropagatorState Expr
-- first check to see if the name is already defined
--   in the current scope
--   if it is, this name should refer to that definition
--   instead of any possible constant definitions
-- if not defined, check to see whether the name is
--   defined as a constant
propConstExpr l@(Literal (Variable v)) = do
    (currNames, constVals) <- get
    return
        if Set.member v currNames then l   -- already defined in scope
        else maybe l id (Map.lookup v constVals)
propConstExpr l@(Literal _)
    = return l
propConstExpr (Unary op expr)
    = Unary op <$> propConstExpr expr
propConstExpr (Binary op e1 e2)
    = Binary op <$> propConstExpr e1 <*> propConstExpr e2
propConstExpr (Call s e)
    = Call s <$> mapM propConstExpr e
propConstExpr (Assign s e)
    = Assign s <$> propConstExpr e
propConstExpr (Lambda s e)
    = Lambda s <$> propConstExpr e
propConstExpr (Subscript s e)
    = Subscript s <$> propConstExpr e
propConstExpr (Ternary e1 e2 e3)
    = Ternary <$> propConstExpr e1 <*> propConstExpr e2 <*> propConstExpr e3










-- Issue 001: [FIXED]
--   constant propagation shouldn't override
--   variable declerations in inner scopes
--```
--     const foo = 42;
--     def a
--     {
--         var foo = 13;
--         print(foo);
--     }
--
--     a();
--     print(foo);
--```
-- should print
--   13
--   42
-- istead of
--   42
--   42

