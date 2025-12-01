module Main where 

import Lexer 
import Parser 
import TypeChecker 
import Interpreter 

main = getContents >>= print . eval . typecheck . parser . lexer 

run :: String -> Expr
run = eval . typecheck . parser . lexer
