-- src/Parser.hs

module Parser where

import Ast
import Control.Applicative (some)
import Data.Either (partitionEithers)
import Text.Parsec
import Text.Parsec (parserFail)
import Text.Parsec.Char (alphaNum, letter, oneOf)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token qualified as Tok

-- -- -- -- Lexer setup -- -- -- --

langDef :: Tok.LanguageDef ()
langDef =
  emptyDef
    { -- Comments
      Tok.commentStart = "/*",
      Tok.commentEnd = "*/",
      Tok.commentLine = "//",
      Tok.nestedComments = True,
      -- Identifier definition
      Tok.identStart = letter,
      Tok.identLetter = alphaNum <|> char '_',
      --  How to start and chain operators
      Tok.opStart = oneOf ".:|=>-",
      Tok.opLetter = oneOf ".:|=>-",
      --  Defined reserved keywords
      Tok.reservedNames =
        [ "new",
          "return",
          "class",
          "extends",
          "let",
          "in",
          "true",
          "false",
          "grant",
          "revoke",
          "setState",
          "bool",
          "int",
          "string",
          "skip",
          "active",
          "notYetActive",
          "suspended",
          "terminated"
        ],
      Tok.reservedOpNames = [".", "(", ")", ",", ";", "=", ":=", "{", "}", "{|", "|}", ":", "|", "=>", "->", "[", "]"],
      --  The language should be case sensitive
      Tok.caseSensitive = True
    }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

identifier :: Parser String
identifier = Tok.identifier lexer

strings :: Parser String
strings = Tok.stringLiteral lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

reservedNames :: String -> Parser ()
reservedNames = Tok.reserved lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

dot :: Parser String
dot = Tok.dot lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

eatWhiteSpace :: Parser ()
eatWhiteSpace = skipMany (oneOf " \n")

-- -- -- -- -- Parser -- -- -- -- --

-- -- -- -- Ground Type

parseGroundType :: Parser GroundType
parseGroundType =
  (reserved "bool" >> return BoolGroundType) <|> (reserved "int" >> return IntGroundType) <|> (reserved "string" >> return StringGroundType) <|> (ClassGroundType <$> identifier)

-- -- -- -- Type

parseType :: Parser Type
parseType = do
  t <- parseGroundType
  s <- option (PurposeSet [] Nothing) parsePurposeSet
  return (Type {gtType = t, piType = s})

-- -- -- -- Values

parseBoolValue :: Parser Value
parseBoolValue = (reserved "true" >> return TrueValue) <|> (reserved "false" >> return FalseValue)

parseIntegerValue :: Parser Value
parseIntegerValue = do
  n <- some digit
  return (IntegerValue (read n :: Integer))

parseStringValue :: Parser Value
parseStringValue = StringValue <$> strings

parseValue :: Parser Value
parseValue = parseBoolValue <|> parseIntegerValue <|> parseStringValue

-- -- -- -- Purposes

symbol :: String -> Parser String
symbol = Tok.symbol lexer

parsePurposeSet :: Parser PurposeSet
parsePurposeSet = between (symbol "{|") (symbol "|}") $ do
  ps <- commaSep identifier <|> return []
  r <- optionMaybe $ try $ do
    reservedOp "|"
    identifier
  return (PurposeSet ps r)

parseState :: Parser Ast.State
parseState =
  (reserved "active" >> return ActiveState)
    <|> (reserved "notYetActive" >> return NotYetActiveState)
    <|> (reserved "suspended" >> return SuspendedState)
    <|> (reserved "terminated" >> return TerminatedState)

parsePurposeState :: Parser PurposeState
parsePurposeState = between (symbol "[") (symbol "]") $ do
  ps <- commaSep identifier
  reservedOp ":"
  s <- parseState
  return $ PurposeState ps s

-- -- -- -- Term

parseBoolTerm :: Parser Term
parseBoolTerm =
  (reserved "true" >> return TrueTerm) <|> (reserved "false" >> return FalseTerm)

parseVarTerm :: Parser Term
parseVarTerm = do
  i <- identifier
  return (VarTerm i)

parseNewTerm :: Parser Term
parseNewTerm = do
  reserved "new"
  p <- parsePurposeSet
  c <- identifier
  args <- parens (commaSep parseTerm)
  return NewTerm {piTerm = p, cTerm = c, argsTerm = args}

parseIntegerTerm :: Parser Term
parseIntegerTerm = do
  n <- Tok.integer lexer
  return (IntegerTerm n)

parseStringTerm :: Parser Term
parseStringTerm = do
  s <- Tok.stringLiteral lexer
  return (StringTerm s)

parseTerm' :: Parser Term
parseTerm' =
  parseNewTerm
    <|> parseVarTerm
    <|> parseBoolTerm
    <|> parseIntegerTerm
    <|> parseStringTerm
    <|> parens parseTerm

parseMethodOrFieldTerm :: Term -> String -> Parser Term
parseMethodOrFieldTerm t i =
  do
    args <- parens (commaSep parseTerm)
    let newTerm = MethodCallTerm {sTerm = t, mTerm = i, argTerm = args}
    dottedTerm newTerm
    <|> do
      let newTerm = FieldAccessTerm {sTerm = t, fTerm = i}
      dottedTerm newTerm

dottedTerm :: Term -> Parser Term
dottedTerm t =
  do
    reservedOp "."
    i <- identifier
    parseMethodOrFieldTerm t i
    <|> return t

parseTerm :: Parser Term
parseTerm = do
  t <- parseTerm'
  dottedTerm t

-- -- -- -- Statements

parseXAssignmentStatements :: Parser Statements
parseXAssignmentStatements = do
  x <- identifier
  reservedOp ":="
  t <- parseTerm
  return (XAssignmentStatements {xStatements = x, tStatements = t})

parseTAssignmentStatements :: Parser Statements
parseTAssignmentStatements = do
  t_f <- parseTerm
  case t_f of
    FieldAccessTerm _ _ -> do
      reservedOp ":="
      t <- parseTerm
      return (TAssignmentStatements {t_fStatements = t_f, tStatements = t})
    _ -> unexpected "ERROR: TermAssignment Requires FieldAccess on Left side"

parseBlockStatement :: Parser Statements
parseBlockStatement = BlockStatements <$> braces parseStatements

parseLetStatements :: Parser Statements
parseLetStatements = do
  reserved "let"
  i <- identifier
  reservedOp ":"
  ty <- parseType
  reservedOp ":="
  t <- parseTerm
  reserved "in"
  s <- parseBlockStatement
  return
    LetStatements
      { xStatements = i,
        tyStatements = ty,
        tStatements = t,
        sStatements = s
      }

parseSkipStatements :: Parser Statements
parseSkipStatements = do
  reserved "skip"
  return SkipStatements

parseReturnStatements :: Parser Statements
parseReturnStatements = do
  reserved "return"
  t <- parseTerm
  return (ReturnStatements {tStatements = t})

parseIfStatements :: Parser Statements
parseIfStatements = do
  reserved "if"
  t <- parseTerm
  reserved "then"
  s0 <- parseBlockStatement
  reserved "else"
  s1 <- parseBlockStatement
  return IfStatements {tStatements = t, s0Statements = s0, s1Statements = s1}

parseDoWhileStatements :: Parser Statements
parseDoWhileStatements = do
  reserved "do"
  s <- parseBlockStatement
  reserved "while"
  t <- parseTerm
  return DoStatements {sStatement = s, tStatements = t}

parseWhileStatements :: Parser Statements
parseWhileStatements = do
  reserved "while"
  t <- parseTerm
  reserved "do"
  s <- parseBlockStatement
  return WhileStatements {tStatements = t, sStatement = s}

parseStatements' :: Parser Statements
parseStatements' =
  parseSkipStatements
    <|> parseReturnStatements
    <|> parseIfStatements
    <|> parseDoWhileStatements
    <|> parseWhileStatements
    <|> parseLetStatements
    <|> try parseTAssignmentStatements
    <|> try parseXAssignmentStatements
    <|> try parseXGrantStatement
    <|> try parseXSetStateStatement
    <|> try parseXRevokeStatement
    <|> try parseMethodCallStatement

parseStatements :: Parser [Statements]
parseStatements =
  many
    ( do
        s <- parseStatements'
        optional (reservedOp ";")
        return s
    )

parseMethodCallStatement :: Parser Statements
parseMethodCallStatement = do
  t <- parseTerm
  case t of
    MethodCallTerm {} -> return MethodCallStatements {tMethodCall = t}
    _ -> unexpected "ERROR: MethodCall expected a call"

parseXGrantStatement :: Parser Statements
parseXGrantStatement = do
  x <- identifier
  reservedOp "."
  reservedNames "grant"
  reservedOp "("
  p <- identifier
  reservedOp ")"
  return XGrantStatements {xStatements = x, pStatements = p}

parseXRevokeStatement :: Parser Statements
parseXRevokeStatement = do
  x <- identifier
  reservedOp "."
  reservedNames "revoke"
  reservedOp "("
  p <- identifier
  reservedOp ")"
  return XRevokeStatements {xStatements = x, pStatements = p}

parseXSetStateStatement :: Parser Statements
parseXSetStateStatement = do
  x <- identifier
  reservedOp "."
  reservedNames "setState"
  reservedOp "("
  p <- parseState
  reservedOp ")"
  return XSetStateStatements {xSetState = x, stSetState = p}

-- -- -- -- TypeVar

parseTypeVar' :: Parser TypeVar
parseTypeVar' = do
  t <- parseType
  x <- identifier
  return TypeVar {tyTypeVar = t, xTypeVar = x}

parseTypeVar :: Parser [TypeVar]
parseTypeVar = do
  tv <- commaSep parseTypeVar'
  reservedOp ")"
  return tv

-- -- -- -- ClassDecl

parseGroundField :: Parser GroundField
parseGroundField = do
  t <- parseGroundType
  i <- identifier
  reservedOp ";"
  return (GroundField {gGroundField = t, fGroundField = i})

parseConstructor :: String -> Parser ConstructorDecl
parseConstructor className = do
  reserved "function"
  name <- identifier
  if name /= className
    then fail $ "ERROR: Class name is " ++ className ++ " but the constructor is named " ++ name
    else do
      ptxpi <- parens (commaSep parseArgumentDecl)
      s <- parseBlockStatement
      return $
        ConstructorDecl
          { mConstructorDecl = className,
            ptxpiConstructorDecl = ptxpi,
            sConstructorDecl = s
          }

parseClassDecl :: Parser ClassDecl
parseClassDecl = do
  reserved "class"
  c <- identifier
  ext <- option "Object" (reserved "extends" >> identifier)
  braces $ do
    fs <- many (try parseGroundField)
    k <- parseConstructor c
    ms <- many parseMethodDecl
    return
      ClassDecl
        { cClassDecl = c,
          extendsClassDecl = ext,
          gtClassDecl = fs,
          kClassDecl = k,
          mClassDecl = ms
        }

-- -- ParseArgumentDecl

parseArgumentDecl :: Parser ArgumentDecl
parseArgumentDecl = do
  i <- identifier
  reservedOp ":"
  t <- parseType
  piOut <- option (piType t) (reservedOp "=>" >> parsePurposeSet)
  return $ ArgumentDecl i t piOut

-- -- -- -- FunctionDecl
parseFunctionDecl :: Parser FunctionDecl
parseFunctionDecl = do
  reserved "function"
  name <- identifier
  args <- parens (commaSep parseArgumentDecl)
  reservedOp "->"
  retTy <- parseType
  body <- parseBlockStatement
  return $
    FunctionDecl
      { tFunctionDecl = retTy,
        mFunctionDecl = name,
        ptxpiFunctionDecl = args,
        sFunctionDecl = body
      }

-- -- -- -- MethodDecl

parseMethodDecl :: Parser MethodDecl
parseMethodDecl = do
  reserved "function"
  startState <- optionMaybe (try parsePurposeState)
  name <- identifier
  endState <- optionMaybe (try parsePurposeState)
  args <- parens (commaSep parseArgumentDecl)
  reservedOp "->"
  retTy <- parseType
  body <- parseBlockStatement
  return $
    MethodDecl
      { tMethodDecl = retTy,
        mMethodDecl = name,
        ptxpiMethodDecl = args,
        sMethodDecl = body,
        sPurposeState = startState,
        fPurposeState = endState
      }

-- -- -- -- ParseProgram
parseProgram :: Parser Program
parseProgram = do
  Tok.whiteSpace lexer -- eatWhiteSpace
  cs <- many parseClassDecl
  eof
  return $ Program cs

-- -- -- -- -- Main   -- -- -- -- --

main = do
  -- let inp = "function aGetter () ->int {|p3|}{return 123;}"
  -- let inp = "function aGetter (a : int {|p0|} => {|p1|}, b : bool {|p2|} => {|p3|}) ->int {|p3|}{return 123;}"
  -- let inp = "class foo extends bar { int a; function foo(a : int {|p0|} => {|p1|} ){ a := a; } function aGetter() -> int {|p3|} { return a; } }"
  -- let inp = "foo.bar.foofoo"
  -- let inp' = parse parseStatements "" inp
  -- print inp'

  -- let inpMethod = "function identity( i : int {|p0|}) -> int {|p0|} {return a;}"
  let inpMethod = "function [R1:active] getObjectID [R1:terminated] () -> int {||} {return ObjectID}"
  let inpMethod' = parse parseMethodDecl "" inpMethod
  print inpMethod'

  -- let inpTrue = "true"
  -- let inpTrue' = parse parseBoolValue "" inpTrue
  -- print inpTrue'

  -- let inpFalse = "false"
  -- let inpFalse' = parse parseBoolValue "" inpFalse
  -- print inpFalse'

  -- let inpInteger = "123"
  -- let inpInteger' = parse parseIntegerValue "" "123"
  -- print inpInteger'

  -- let inpSkip = "skip"
  -- let inpSkip' = parse parseSkipStatements "" inpSkip
  -- print inpSkip'

  -- let inpPurposeSet = "{|p0, p1, p2|}"
  -- let inpPurposeSet' = parse parsePurposeSet "" inpPurposeSet
  -- print inpPurposeSet'

  -- let inpPurposeSetRho = "{|p0, p1, p2| RHOOOO |}"
  -- let inpPurposeSetRho' = parse parsePurposeSet "" inpPurposeSetRho
  -- print inpPurposeSetRho'

  -- let inpGroundTypeBool = "bool"
  -- let inpGroundTypeBool' = parse parseGroundType "" inpGroundTypeBool
  -- print inpGroundTypeBool'

  -- let inpGroundTypeInt = "int"
  -- let inpGroundTypeInt' = parse parseGroundType "" inpGroundTypeInt
  -- print inpGroundTypeInt'

  -- let inpGroundTypeSomeClass = "someClass"
  -- let inpGroundTypeSomeClass' = parse parseGroundType "" inpGroundTypeSomeClass
  -- print inpGroundTypeSomeClass'

  -- let inpTypeInt = "int {|p0, p1|}"
  -- let inpTypeInt' = parse parseType "" inpTypeInt
  -- print inpTypeInt'

  -- let inpAssignmentStatement = "x := y"
  -- let inpAssignmentStatement' = parse parseXAssignmentStatements "" inpAssignmentStatement
  -- print inpAssignmentStatement'

  -- let inpFieldAssignmentStatement = "foo.bar := y"
  -- let inpFieldAssignmentStatement' = parse parseTAssignmentStatements "" inpFieldAssignmentStatement
  -- print inpFieldAssignmentStatement'

  -- let inpCommaSepStatements = "x := y; y := x"
  -- let inpCommaSepStatements' = parse parseStatements "" inpCommaSepStatements
  -- print inpCommaSepStatements'

  -- let inpLetStatement = "let x : Bool {|p0|} := true in {x := false; x := x; skip; skip}"
  -- let inpLetStatement' = parse parseLetStatements "" inpLetStatement
  -- print inpLetStatement'

  -- let inpIfStatement = "if true then { return true; x:= 123 } else { return false; y := 654 }"
  -- let inpIfStatement' = parse parseIfStatements "" inpIfStatement
  -- print inpIfStatement'

  -- let inpDoWhileStatement = "do {x := 1; y:= 2 } while (true)"
  -- let inpDoWhileStatement' = parse parseDoWhileStatements "" inpDoWhileStatement
  -- print inpDoWhileStatement'

  -- let inpXGrant = "x.grant(SomeRow)"
  -- let inpXGrant' = parse parseXGrantStatement "" inpXGrant
  -- print inpXGrant'

  -- let inpXGrant = "x.y.z.grant(SomeRow)"
  -- let inpXGrant' = parse parseXGrantStatement "" inpXGrant
  -- print inpXGrant'

  --  let inpXRevoke = "y.revoke(SomeOtherRow)"
  --  let inpXRevoke' = parse parseXRevokeStatement "" inpXRevoke
  --  print inpXRevoke'

  --  let inpArgumentDeclInt = "a : int {|foo, bar|}"
  --  let inpArgumentDeclInt' = parse parseArgumentDecl "" inpArgumentDeclInt
  --  print inpArgumentDeclInt'

  --  let inpFunctionEmpty = "function foo() -> int {||} { return a; }"
  --  let inpFunctionEmpty' = parse parseFunctionDecl "" inpFunctionEmpty
  --  print inpFunctionEmpty'

  --  let inpFunctionSingle = "function foo(a : int {|p0, p1|} => {|p0, p1|}) -> int {|FOO, BAR|} { return a; }"
  --  let inpFunctionSingle' = parse parseFunctionDecl "" inpFunctionSingle
  --  print inpFunctionSingle'

  --  let inpFunctionDouble = "function foo(a : int {|p0, p1|} => {|p0, p1|}, b : bool {|p0|} => {|p1|} ) -> int {|FOO, BAR|} { return a; }"
  --  let inpFunctionDouble' = parse parseFunctionDecl "" inpFunctionDouble
  --  print inpFunctionDouble'

  --  let inpClass = "class foo extends bar {int a; function foo(a : int {|p0|} => {|p1|} -> int {|p2|} ) {return a;}; }"
  --  print inpClass
  --  let inpClass' = parse parseClassDecl "" inpClass
  --  print inpClass'

  --  let inpMethod = "Bool {p0} fib({p0} Int {p4} y){let x : Bool {p3} = y}"
  --  let inpMethod' = parse parseMethod "" inpMethod
  --  print inpMethod'

  putStrLn ""
