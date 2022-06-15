
{-# OPTIONS_GHC  -Wall -O2 #-}

module Main where

import Language.Types        (Stmt)
import Language.Parser       (parseFile)
import Language.Interpreter  (runProgram)
import Language.Validate     (validate)
import Language.Optimizer    (optimize)

import Data.Functor ((<&>))

import System.Environment (getArgs)


main :: IO ()
main = getArgs >>= dispatch


dispatch :: [FilePath] -> IO ()
dispatch [filename] = readFile filename <&> parseFile
                  >>= either (printFail "parse error") execute
dispatch _          = putStrLn "usage: main [filename]"


printFail :: Show a => String -> a -> IO b
printFail errMsg msg = print msg >> fail errMsg


execute :: Stmt -> IO ()
execute ast = validate ast <&> fmap optimize
          >>= either (printFail "invalid code") debug
          >>= runProgram
          >>= either print return


debug :: Show a => a -> IO a
-- debug = (\e -> print e >> putStrLn "" >> return e)
debug = return

