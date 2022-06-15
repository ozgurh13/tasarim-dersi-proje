
{-# OPTIONS_GHC  -Wall -O2 #-}

module Language.Types where

import Data.Bool (bool)
import Data.List ((\\))

import Data.Map.Strict (Map)

data Stmt
    = Seq [Stmt]
    | Block Stmt
    | If Expr Stmt Stmt
    | While Expr Stmt
    | Expr Expr
    | ConstDecl String Expr
    | VarDecl String Expr
    | FunDecl String [String] Stmt
    | TryExcept Stmt Stmt
    | Return Expr
    | Break
    | Pass
    deriving (Show, Eq)



data Expr
    = Literal Object
    | Unary UnOp Expr
    | Binary BinOp Expr Expr
    | Call String [Expr]
    | Assign String Expr
    | Lambda [String] Expr
    | Subscript String Expr
    | Ternary Expr Expr Expr
    deriving (Show, Eq)


data UnOp = Negate | Not deriving (Show, Eq)

data BinOp = CmpEQ | CmpNE | CmpLT | CmpLE | CmpGT | CmpGE
           | And | Or
           | Add | Sub | Mul | Div | Mod | Pow
           | ShiftL | ShiftR
           deriving (Show, Eq)









data Object
    = Integer  !Integer
    | Double   !Double
    | Bool     !Bool
    | String   !String
    | Variable !String
    --           Params  Body      Environment
    | Function ![String] Stmt ![Map String Object]
    | List     ![Expr]
    | Null


instance Show Object where
    show (Integer i)      = show i
    show (Double d)       = show d
    show (Bool b)         = bool "false" "true" b
    show (String s)       = s
    show (Variable v)     = v
    show (Function _ _ _) = "<function>"
    show (List list)      = showItems list
    show Null             = "null"

showItems :: [Expr] -> String
showItems = show . map showItem
  where showItem (Literal l)  = l
        showItem (Lambda _ _) = String "<lambda>"
        showItem (Call _ _)   = String "<call>"
        showItem _            = String "..."


instance Eq Object where
    (==) (Integer i1) (Integer i2) = i1 == i2
    (==) (Integer i1) (Double d2)  = fromIntegral i1 == d2
    (==) (Double d1)  (Integer i2) = d1 == fromIntegral i2
    (==) (Double d1)  (Double d2)  = d1 == d2
    (==) (String s1)  (String s2)  = s1 == s2
    (==) (Bool b1)    (Bool b2)    = b1 == b2
    (==) (List l1)    (List l2)    = l1 == l2
    (==) Null         Null         = True
    (==) _            _            = False


instance Ord Object where
    (<=) (Integer i1) (Integer i2) = i1 <= i2
    (<=) (Integer i1) (Double d2)  = fromIntegral i1 <= d2
    (<=) (Double d1)  (Integer i2) = d1 <= fromIntegral i2
    (<=) (Double d1)  (Double d2)  = d1 <= d2
    (<=) _            _            = undefined


instance Num Object where
    (+) (Integer i1) (Integer i2) = Integer (i1 + i2)
    (+) (Integer i1) (Double d2)  = Double (fromIntegral i1 + d2)
    (+) (Double d1)  (Integer i2) = Double (d1 + fromIntegral i2)
    (+) (Double d1)  (Double d2)  = Double (d1 + d2)
    (+) (String s1)  (String s2)  = String (s1 ++ s2)
    (+) (List l1)    (List l2)    = List (l1 ++ l2)
    (+) _            _            = Null

    (-) (Integer i1) (Integer i2) = Integer (i1 - i2)
    (-) (Integer i1) (Double d2)  = Double (fromIntegral i1 - d2)
    (-) (Double d1)  (Integer i2) = Double (d1 - fromIntegral i2)
    (-) (Double d1)  (Double d2)  = Double (d1 - d2)
    (-) (List l1)    (List l2)    = List (l1 \\ l2)
    (-) _            _            = Null

    (*) (Integer i1) (Integer i2) = Integer (i1 * i2)
    (*) (Integer i1) (Double d2)  = Double (fromIntegral i1 * d2)
    (*) (Double d1)  (Integer i2) = Double (d1 * fromIntegral i2)
    (*) (Double d1)  (Double d2)  = Double (d1 * d2)
    (*) (List l1)    (List l2)    = List [ Literal (List [a, b]) | a <- l1, b <- l2 ]
    (*) _            _            = Null

    negate (Integer i) = Integer (negate i)
    negate (Double d)  = Double (negate d)
    negate (Bool b)    = Bool (not b)
    negate _           = undefined

    abs (Integer i) = Integer (abs i)
    abs (Double d)  = Double (abs d)
    abs _           = undefined

    signum (Integer i) = Integer (signum i)
    signum (Double d)  = Double (signum d)
    signum _           = undefined

    fromInteger = undefined

