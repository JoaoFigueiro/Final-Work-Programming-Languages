{
module Parser where 

import Lexer 
}

%name parser 
%tokentype { Token }
%error { parseError }

%left '+' '-'
%left '*'

%token 
    num             { TokenNum $$ }
    var             { TokenVar $$ }
    true            { TokenTrue }
    false           { TokenFalse }
    '+'             { TokenPlus }
    '*'             { TokenTimes }
    "&&"            { TokenAnd }
    "||"            { TokenOr }
    '('             { TokenLParen }
    ')'             { TokenRParen }
    if              { TokenIf }
    then            { TokenThen }
    else            { TokenElse }
    '\\'            { TokenLam }
    "->"            { TokenArrow }
    ':'             { TokenColon }
    Bool            { TokenBoolean }
    Num             { TokenNumber }
    ','             { TokenComma }
    fst             { TokenFst }
    snd             { TokenSnd }

%nonassoc if then else '\\' "->"
%left "||"
%left "&&"
%left '+'
%left '*'
%left APP
%left fst snd

%% 

Exp     : num           { Num $1 }
        | true          { BTrue }
        | false         { BFalse }
        | var           { Var $1 }
        | Exp '+' Exp   { Add $1 $3 }
        | Exp '*' Exp   { Times $1 $3 }
        | Exp "&&" Exp  { And $1 $3 }
        | Exp "||" Exp  { Or $1 $3 }
        | if Exp then Exp else Exp      { If $2 $4 $6 }
        | '\\' var ':' Type "->" Exp    { Lam $2 $4 $6 }
        | Exp Exp %prec APP             { App $1 $2 }
        | fst Exp                       { Fst $2 }
        | snd Exp                       { Snd $2 }
        | '(' Exp ',' Exp ')'           { Pair $2 $4 }
        | '(' Exp ')'                   { Paren $2 }

Type    : Bool                          { TBool }
        | Num                           { TNum }
        | '(' Type ',' Type ')'         { TPair $2 $4 } -- Tipo Tupla
        | Type "->" Type                { TFun $1 $3 }
        | '(' Type ')'                  { $2 }

{

parseError :: [Token] -> a 
parseError _ = error "Syntax error!"

}