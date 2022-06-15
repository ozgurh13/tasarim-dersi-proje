
{-# OPTIONS_GHC  -Wall -O2 #-}

module Language.Environment
( Environment
, newEnv
, get
, define
, defined
, assign
) where

import Language.Types

import Data.Composition ((.:))
import qualified Data.Map.Strict as Map

type Env = Map.Map String Object
type Environment = [Env]

newEnv :: Env
newEnv = Map.empty

define :: String -> Object -> Env -> Env
define = Map.insert

get :: String -> Environment -> Maybe Object
get _    []          = Nothing
get name (curr:prev) = maybe (get name prev) Just (Map.lookup name curr)

defined :: String -> Environment -> Bool
defined = maybe False (const True) .: get

assign :: String -> Object -> Environment -> Environment
assign _ _ [] = error "assign"
assign name value (curr:prev)
    | Map.member name curr
    = define name value curr : prev
    | otherwise
    = curr : assign name value prev

