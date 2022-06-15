
{-# OPTIONS_GHC  -O2 #-}

module Language.Parser (parseFile) where

{-
 | Parser
 |   parse a file into the corresponding AST
 |   the only other responsibility of this file
 |       is to report syntax errors
 -}

import Language.Types

import           Text.Parsec
import           Text.Parsec.Expr
import qualified Text.Parsec.Token    as Token
import           Text.Parsec.Language

import Data.Maybe (fromMaybe)


type Parser = Parsec String ()


parseFile = parse (padSpaces parseProgram <* eof) ""


parseProgram :: Parser Stmt
parseProgram = Seq <$> many (padSpaces decleration)

decleration :: Parser Stmt
decleration =  parseVarDecl
           <|> parseFunDecl
           <|> parseConstDecl
           <|> statements

-- `const e = 2.71828;`
parseConstDecl = ConstDecl <$> (reserved "const" *> identifier <* reservedOp "=")
                           <*> (padSpaces expression <* char ';')

-- `var a, b = 2, c = b;`
parseVarDecl = do
    decls <- parseVarDeclMany <* char ';'
    return (Seq decls)

parseVarDeclMany = map (uncurry VarDecl) <$>
        (reserved "var" >> sepBy parseVarDecl' (padChar ','))

parseVarDecl' = (,) <$> padSpaces identifier
                    <*> (  reservedOp "=" *> padSpaces expression
                       <|> return (Literal Null) )


parseFunDecl = FunDecl <$> (reserved "def" *> padSpaces identifier)
                       <*> (sepBy identifier whiteSpace)
                       <*> block

statements :: Parser Stmt
statements =  block
          <|> ifStmt
          <|> forStmt
          <|> whileStmt
          <|> try doWhileStmt
          <|> untilStmt
          <|> doUntilStmt
          <|> loopStmt
          <|> returnStmt
          <|> switchStmt
          <|> matchStmt
          <|> breakStmt
          <|> tryExceptStmt
          <|> exprStmt
          <|> passStmt

block = Block <$> braces parseProgram

ifStmt = If <$> (reserved "if" *> padSpaces expression)
            <*> padSpaces statements
            <*> (  reserved "else" *> padSpaces statements
               <|> return Pass )

forStmt = do
    reserved "for"
    initializer <- padSpaces (  Expr <$> expression
                            <|> (Seq <$> parseVarDeclMany)
                            <|> return Pass )
    char ':'
    condition <- padSpaces (optionMaybe expression)
    char ':'
    increment <- padSpaces (optionMaybe expression)
    reserved "do"
    body <- padSpaces statements
    return $ Block (Seq [ initializer        -- see [For constructor]
                        , While (fromMaybe (Literal $ Bool True) condition)
                                (Seq [body, maybe Pass Expr increment])])
{-
 | [For constructor]
 |    there isn't a designated constructor for For as
 |       for (a; b; c) d
 |       ==> For a b c d
 |    is the same as
 |       { a; while b: { d; c; } }
 |       ==> Block (Seq [a, While b (Seq [d, c])])
 -}


whileStmt = While <$> (reserved "while" *> padSpaces expression)
                  <*> padSpaces statements

doWhileStmt = do
    reserved "do"
    stmt <- padSpaces statements
    reserved "while"
    expr <- padSpaces expression
    return $ Seq [stmt, While expr stmt]


untilStmt = While <$> (reserved "until" *>
                            (Unary Not <$> padSpaces expression))
                  <*> padSpaces statements

doUntilStmt = do
    reserved "do"
    stmt <- padSpaces statements
    reserved "until"
    expr <- padSpaces expression
    return $ Seq [stmt, While (Unary Not expr) stmt]


-- loop => while true
loopStmt = While (Literal (Bool True))
        <$> (reserved "loop" *> padSpaces statements)


returnStmt = Return <$> (reserved "return" *>
                              (expression <|> return (Literal Null))
                        <* padChar ';')

switchStmt = do
    reserved "switch"
    char '|'
    cases <- sepBy ((,) <$> (padSpaces expression <|> return trueLit)
                        <*> (padChar ':' *> statements))
                   (padChar '|')
    return $ desugarSwitch cases        -- see [Switch constructor]
    where
        desugarSwitch [] = Pass
        desugarSwitch ((e,s):es)
            | e == trueLit = s
            | otherwise    = If e s (desugarSwitch es)

        trueLit = Literal (Bool True)
{-
 | [Switch constructor]
 |   there isn't a designated constructor for Switch as it
 |     desugars to a series of nested If constructors
 |
 |   a worked example
 |      switch
 |        | a == 2
 |        : print("a is 2");
 |
 |        | b != 1
 |        : { launchMissles(); hideInBunker(); }
 |
 |        |: { eatIceCreamWith("Simon Peyton Jones"); }
 |
 |      ==> desugars to
 |        if a == 2
 |            print("a is 2");
 |        else if b != 1 {
 |            launchMissles();
 |            hideInBunker();
 |        }
 |        else {
 |            eatIceCreamWith("Simon Peyton Jones");
 |        }
 |
 |
 |      ==> AST
 |        If (Binary CmpEQ (Literal a) (Literal 2))
 |           (Expr (Call "print" [Literal "a is 2"]))
 |           (If (Binary CmpNE (Literal b) (Literal 1))
 |               (Block (Seq [ Expr (Call "launchMissles" [])
 |                           , Expr (Call "hideInBunker" [])]))
 |               (Block (Seq [Expr (Call "eatIceCreamWith" [Literal "Simon Peyton Jones"])])))
 -}


matchStmt = do
    reserved "match"
    e' <- padSpaces expression
    char '|'
    cases <- sepBy ((,) <$> (padSpaces expression <|> return tmpVar)
                        <*> (padChar ':' *> statements))
                   (padChar '|')
    return $ Seq [ VarDecl tmpVarName e', desugarMatch cases ]      -- see [Match constructor]
    where
        desugarMatch :: [(Expr, Stmt)] -> Stmt
        desugarMatch [] = Pass
        desugarMatch ((e,s):es)
            | e == tmpVar = s
            | otherwise   = If (Binary CmpEQ tmpVar e) s (desugarMatch es)

        tmpVar = Literal (Variable tmpVarName)
        tmpVarName = "%match"
{-
 | [Match constructor]
 |   there isn't a designated constructor for Match as it
 |     desugars to a series of nested If constructors
 |
 |  N.B. because the expression may need to be further evaluated
 |         (and the computation may be expensive) we should
 |         evaluate it once and save the result in a variable
 |       the variable name chosen is "%match" as it's can't be
 |         defined by the user, thus avoiding potential name clashes
 |
 |  N.B. empty cases in match evaluate to true
 |       however, we can't just plop (Literal (Bool True)) where
 |         we see an empty case as later on we compare the expression
 |         with the value of the newly assigned "%match" variable
 |       this would introduce a bug, as it will check if the value of
 |         "%match" is true, which isn't what we want
 |       it also would clash with situation where the user is actually
 |         comparing the value to true, which in result would interfere
 |         with optimizing the empty case to just yield true, as we don't
 |         want to optimize away a compare to the value true
 |       here we will use the the variable "%match" in the empty case
 |         and compare the value with itself, which will evaluate to
 |         true, so we can just get rid of the compare and just replace
 |         the expression with the literal true
 |
 |
 |   a worked example
 |      match a
 |        | 2
 |        : print("a is 2");
 |
 |        | 1
 |        : print("a is 1");
 |
 |        |: print("its neither");
 |
 |      ==> desugars to
 |        if a == 2
 |            print("a is 2");
 |        else if a == 1
 |            print("a is 1");
 |        else
 |            print("its neither");
 -}


breakStmt = reserved "break" *> padChar ';' *> return Break


tryExceptStmt = TryExcept <$> (reserved "try"    *> padSpaces statements)
                          <*> (reserved "except" *> padSpaces statements)


exprStmt = Expr <$> expression <* padChar ';'


passStmt = padChar ';' *> return Pass


expression :: Parser Expr
expression = binaryExpression


binaryExpression :: Parser Expr
binaryExpression = buildExpressionParser binaryOperators binaryTerm

binaryOperators =
    [ [Prefix (reservedOp "-"  >> return (Unary Negate ))           ,
       Prefix (reservedOp "!"  >> return (Unary Not    ))           ]

    , [Infix  (reservedOp ">>" >> return (Binary ShiftR)) AssocLeft ,
       Infix  (reservedOp "<<" >> return (Binary ShiftL)) AssocLeft ,
       Infix  (reservedOp "**" >> return (Binary Pow   )) AssocRight]

    , [Infix  (reservedOp "*"  >> return (Binary Mul   )) AssocLeft ,
       Infix  (reservedOp "/"  >> return (Binary Div   )) AssocLeft ,
       Infix  (reservedOp "%"  >> return (Binary Mod   )) AssocLeft ]

    , [Infix  (reservedOp "+"  >> return (Binary Add   )) AssocLeft ,
       Infix  (reservedOp "-"  >> return (Binary Sub   )) AssocLeft ]

    , [Infix  (reservedOp ">"  >> return (Binary CmpGT )) AssocLeft ,
       Infix  (reservedOp ">=" >> return (Binary CmpGE )) AssocLeft ,
       Infix  (reservedOp "<"  >> return (Binary CmpLT )) AssocLeft ,
       Infix  (reservedOp "<=" >> return (Binary CmpLE )) AssocLeft ]

    , [Infix  (reservedOp "==" >> return (Binary CmpEQ )) AssocLeft ,
       Infix  (reservedOp "!=" >> return (Binary CmpNE )) AssocLeft ]

    , [Infix  (reservedOp "&&" >> return (Binary And   )) AssocRight]
    , [Infix  (reservedOp "||" >> return (Binary Or    )) AssocRight]
    ]

binaryTerm =  parens binaryExpression
          <|> parseLiteral
          <|> parseTernary
          <|> parseLambda
          <|> try parseCall
          <|> try parseAssign
          <|> try parseSubscript
          <|> Literal . Variable <$> identifier


-- ternary  =>  `? bool_expr : expr : expr`
parseTernary = Ternary
            <$> (char '?' *> padSpaces expression)
            <*> (char ':' *> padSpaces expression)
            <*> (char ':' *> padSpaces expression)

-- lambda  =>  `\'arg1 'arg2 ... -> expr`
parseLambda = Lambda <$> (reservedOp "\\" *> sepBy identifier whiteSpace)
                     <*> (reservedOp "->" *> padSpaces expression)

parseLiteral = Literal
            <$> padSpaces (  parseNull
                         <|> parseNumber
                         <|> parseBool
                         <|> parseString
                         <|> parseList )

parseNull = reserved "null" *> return Null
parseNumber = either Integer Double <$> naturalOrFloat

parseString = String <$> stringLiteral

parseBool = Bool <$> choice [ reserved "true"  >> return True
                            , reserved "false" >> return False ]

parseList = List <$> brackets (sepBy expression (padChar ','))

parseCall = Call <$> identifier <*> parens (sepBy expression (padChar ','))

parseAssign = Assign <$> padSpaces identifier
                     <*> (reservedOp "=" *> padSpaces expression)

parseSubscript = Subscript <$> identifier <*> brackets expression















padSpaces x = whiteSpace *> x <* whiteSpace

padChar = padSpaces . char



identifier     = Token.identifier       lexer       -- parses an identifier
reserved       = Token.reserved         lexer       -- parses a reserved name
reservedOp     = Token.reservedOp       lexer       -- parses an operator
parens         = Token.parens           lexer       -- parses surrounding parenthesis
braces         = Token.braces           lexer       -- parses surrounding braces
brackets       = Token.brackets         lexer       -- pasese surrounding brackets
semi           = Token.semi             lexer       -- parses a semicolon
whiteSpace     = Token.whiteSpace       lexer       -- parses whitespace
naturalOrFloat = Token.naturalOrFloat   lexer       -- parses a natural number or float
stringLiteral  = Token.stringLiteral    lexer       -- parses a string literal

natural        = Token.natural          lexer       -- parses a natural number
float          = Token.float            lexer       -- parses a float
dot            = Token.dot              lexer       -- parses a dot


lexer = Token.makeTokenParser languageDef


languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.nestedComments  = False
           , Token.identStart      = letter <|> char '_'
           , Token.identLetter     = alphaNum <|> char '_'
           , Token.reservedNames   = [ "true", "false"
                                     , "if", "else"
                                     , "while", "for", "do", "until"
                                     , "var", "null", "const"
                                     , "def", "return"
                                     , "switch", "match", "loop"
                                     , "break", "continue"   -- TODO: continue, need extra constructor for For
                                     , "try", "except" ]
           , Token.reservedOpNames = [ "-", "!", "="
                                     , ">>", "<<", "**"
                                     , "*", "/", "+", "-", "%"
                                     , ">", ">=", "<", "<="
                                     , "==", "!="
                                     , "&&" , "||"
                                     , "\\", "->" ]
           , Token.caseSensitive   = True }

