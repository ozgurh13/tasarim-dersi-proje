
{-# OPTIONS_GHC  -Wall -O2 #-}

module Language.Validate (validate) where

{-
 | Validate
 |   checks if the code is semantically valid
 |
 |   current checks
 |      -> no return statements outside of functions
 |      -> no break statements outside of loops
 |      -> variables and constants are only declared once in a given scope
 |      -> there aren't name clashes between vars and consts
 |      -> there aren't name clashes between funcs and consts
 -}


import Language.Types
import Language.Optimizer (simplify)

import Control.Monad.State
import Control.Monad.Except

import qualified Data.Set as Set



validate :: Stmt -> IO (Either ValidatorError Stmt)
validate = validate' . simplify



validate' :: Stmt -> IO (Either ValidatorError Stmt)
validate' ast = evalStateT (runExceptT (validateStmt ast))
    (ValidatorState 0 0 newDecls)




type Validator = ExceptT ValidatorError (StateT ValidatorState IO)

data ValidatorError
    = BreakStmt String
    | ReturnStmt String
    | ConstError String
    | ConflictingIdentifier String
    | RedefinedVar String
    | RedefinedConst String
    deriving (Show)



--                                    func  loop
data ValidatorState = ValidatorState !Word !Word Decls

type Decls      = (DeclsVar, DeclsConst, DeclsFunc)
type DeclsVar   = Set.Set String
type DeclsConst = Set.Set String
type DeclsFunc  = Set.Set String

newDecls :: Decls
newDecls = (Set.empty, Set.empty, Set.empty)


-- keep track of depth of loops and functions
-- if the depth is 0, we aren't in a loop/func
incFunc, incLoop :: ValidatorState -> ValidatorState
decFunc, decLoop :: ValidatorState -> ValidatorState
getFunc, getLoop :: ValidatorState -> Word

incFunc (ValidatorState f l c) = ValidatorState (succ f) l c
decFunc (ValidatorState f l c) = ValidatorState (pred f) l c
getFunc (ValidatorState f _ _) = f

incLoop (ValidatorState f l c) = ValidatorState f (succ l) c
decLoop (ValidatorState f l c) = ValidatorState f (pred l) c
getLoop (ValidatorState _ l _) = l





-- when declaring a variable
--   it mustn't already be declared
--   it mustn't already be declared as const
--   it can already be declared as func
-- in the same scope
validateVar :: String -> ValidatorState -> Either ValidatorError ValidatorState
validateVar v (ValidatorState f l (vars, consts, funcs))
    | v `Set.member` consts
    = Left $ ConstError $ v ++ " is already declared as const"

    | v `Set.member` vars
    = Left $ RedefinedVar $ v ++ " is already defined"

    | otherwise    -- don't need to check if a func with that name already exists
    = Right $ ValidatorState f l (v `Set.insert` vars, consts, funcs)

-- when declaring a constant
--   it mustn't already be declared
--   it mustn't already be declared as var
--   it mustn't already be declared as func
-- in the same scope
validateConst :: String -> ValidatorState -> Either ValidatorError ValidatorState
validateConst c (ValidatorState f l (vars, consts, funcs))
    | c `Set.member` vars
    = Left $ ConflictingIdentifier $ c ++ " is defined as both var and const"

    | c `Set.member` consts
    = Left $ RedefinedConst $ c ++ " is already defined"

    | c `Set.member` funcs
    = Left $ ConflictingIdentifier $ c ++ " is defined as both func and const"

    | otherwise
    = Right $ ValidatorState f l (vars, c `Set.insert` consts, funcs)

-- when declaring a function
--   it mustn't already be declared as const
--   it can already be declared as var
validateFunc :: String -> ValidatorState -> Either ValidatorError ValidatorState
validateFunc g (ValidatorState f l (vars, consts, funcs))
    | g `Set.member` consts
    = Left $ ConstError $ g ++ " already declared as const"

    | otherwise    -- don't need to check if a var/func with that name already exists
    = Right $ ValidatorState f l (vars, consts, g `Set.insert` funcs)








validateStmt :: Stmt -> Validator Stmt

validateStmt (Seq s) = Seq <$> traverse validateStmt s

validateStmt (Block block) = do
    ValidatorState f l gc <- get
    put $ ValidatorState f l newDecls
    b <- validateStmt block
    ValidatorState f' l' _ <- get
    put $ ValidatorState f' l' gc    -- f and f' (and l and l') should be the same
    return $ Block b

validateStmt (If b ts fs) = If b <$> validateStmt ts <*> validateStmt fs

validateStmt (While c s) = do
    modify incLoop
    s' <- validateStmt s
    modify decLoop
    return $ While c s'

validateStmt (Expr (Assign str val)) = do
    ValidatorState _ _ (_, gC, _) <- get
    if str `Set.member` gC then
        throwError . ConstError $
            "const value " ++ str ++ " doesn't support assignment"
    else return $ Expr (Assign str val)
validateStmt (Expr e) = return $ Expr e

validateStmt (VarDecl v e) = do
    curr <- get
    either throwError put $ validateVar v curr
    return $ VarDecl v e

validateStmt (ConstDecl str value) = do
    curr <- get
    either throwError put $ validateConst str curr
    return $ ConstDecl str value

validateStmt (FunDecl f a s) = do
    curr <- get
    either throwError put $ validateFunc f curr
    modify incFunc
    s' <- validateStmt s
    modify decFunc
    return $ FunDecl f a s'

validateStmt (TryExcept t e) = TryExcept <$> validateStmt t <*> validateStmt e

validateStmt (Return v) = do
    val <- gets getFunc
    when (val == 0) (throwError $ ReturnStmt "cannot return from outside of a function")
    return $ Return v

validateStmt Break = do
    val <- gets getLoop
    when (val == 0) (throwError $ BreakStmt "cannot break from outside of a loop")
    return Break

validateStmt Pass = return Pass

