module PrettyPrinter where

import Ast
import Parser hiding (main)
import Text.Parsec (parse)

tab :: String
tab = "     " -- Tab is equivalent to around 5 spaces for now
-- TODO(Joan) Account correct Ident - Joan

prettyPrinterPurposeSet :: PurposeSet -> String
prettyPrinterPurposeSet (PurposeSet ps r) =
  "{| " ++ prettyPrinterPurposeSet' ps ++ rho r ++ " |}"
  where
    prettyPrinterPurposeSet' [] = ""
    prettyPrinterPurposeSet' [x] = x
    prettyPrinterPurposeSet' (x : xs) = x ++ ", " ++ prettyPrinterPurposeSet' xs

    rho Nothing = ""
    rho (Just rho) = " | " ++ rho

prettyPrinterTerm :: Term -> String
prettyPrinterTerm (VarTerm x) = x
prettyPrinterTerm (StringTerm str) = "\"" ++ str ++ "\""
prettyPrinterTerm (FieldAccessTerm s f) = prettyPrinterTerm s ++ "." ++ f
prettyPrinterTerm (MethodCallTerm s m args) =
  prettyPrinterTerm s ++ "." ++ m ++ "(" ++ prettyPrinterTerm' args ++ ")"
  where
    prettyPrinterTerm' [] = ""
    prettyPrinterTerm' [t] = prettyPrinterTerm t
    prettyPrinterTerm' (t : ts) = prettyPrinterTerm t ++ ", " ++ prettyPrinterTerm' ts
prettyPrinterTerm (NewTerm ps c args) =
  "new " ++ prettyPrinterPurposeSet ps ++ " " ++ c ++ "(" ++ prettyPrinterTerm' args ++ ")"
  where
    prettyPrinterTerm' [] = ""
    prettyPrinterTerm' [t] = prettyPrinterTerm t
    prettyPrinterTerm' (t : ts) = prettyPrinterTerm t ++ ", " ++ prettyPrinterTerm' ts
prettyPrinterTerm (TrueTerm) = "true"
prettyPrinterTerm (FalseTerm) = "false"
prettyPrinterTerm (IntegerTerm n) = show n

prettyPrinterValue :: Value -> String
prettyPrinterValue TrueValue = "true"
prettyPrinterValue FalseValue = "false"
prettyPrinterValue (IntegerValue n) = show n
prettyPrinterValue (StringValue s) = s
prettyPrinterValue _ = show "ERROR"

prettyPrinterGroundType :: GroundType -> String
prettyPrinterGroundType BoolGroundType = "bool"
prettyPrinterGroundType IntGroundType = "int"
prettyPrinterGroundType StringGroundType = "string"
prettyPrinterGroundType UnitGroundType = "Unit"
prettyPrinterGroundType (ClassGroundType c) = show c

prettyPrinterType :: Type -> String
prettyPrinterType (Type gt pi) = (prettyPrinterGroundType gt) ++ " " ++ (prettyPrinterPurposeSet pi)

prettyPrinterStatements :: Statements -> String
prettyPrinterStatements (XAssignmentStatements x t) = x ++ " := " ++ prettyPrinterTerm t
prettyPrinterStatements (TAssignmentStatements t_f t) = (prettyPrinterTerm t_f) ++ " := " ++ prettyPrinterTerm t
prettyPrinterStatements SkipStatements = "skip"
prettyPrinterStatements (BlockStatements b) = "{\n" ++ tab ++ prettyPrinterStatements' b ++ "}"
  where
    prettyPrinterStatements' [] = ""
    prettyPrinterStatements' [t] = tab ++ prettyPrinterStatements t ++ ";\n"
    prettyPrinterStatements' (t : ts) = "" ++ prettyPrinterStatements t ++ ";\n" ++ prettyPrinterStatements' ts
prettyPrinterStatements (LetStatements x ty t s) = "let " ++ x ++ " : " ++ (prettyPrinterType ty) ++ " := " ++ (prettyPrinterTerm t) ++ " in " ++ (prettyPrinterStatements s)
prettyPrinterStatements (IfStatements t s0 s1) = "if ( " ++ (prettyPrinterTerm t) ++ " )\nthen\n" ++ (prettyPrinterStatements s0) ++ "\nelse\n" ++ (prettyPrinterStatements s1)
prettyPrinterStatements (DoStatements s t) = "do\n" ++ (prettyPrinterStatements s) ++ "\nwhile ( " ++ (prettyPrinterTerm t) ++ " )"
prettyPrinterStatements (WhileStatements t s) = "while ( " ++ (prettyPrinterTerm t) ++ " )\ndo\n" ++ (prettyPrinterStatements s)
prettyPrinterStatements (XGrantStatements x p) = x ++ ".grant(" ++ p ++ ")"
prettyPrinterStatements (XRevokeStatements x p) = x ++ ".revoke(" ++ p ++ ")"
prettyPrinterStatements (ReturnStatements t) = "return " ++ prettyPrinterTerm t
prettyPrinterStatements (MethodCallStatements t) = prettyPrinterTerm t

prettyPrinterArgumentDecl :: ArgumentDecl -> String
prettyPrinterArgumentDecl (ArgumentDecl n t pi) = n ++ " : " ++ (prettyPrinterType t) ++ " => " ++ (prettyPrinterPurposeSet pi)

prettyPrinterArgumentDeclSet :: [ArgumentDecl] -> String
prettyPrinterArgumentDeclSet [] = "()"
prettyPrinterArgumentDeclSet args = "(" ++ (prettyPrinterArgumentDeclSet' args)
  where
    prettyPrinterArgumentDeclSet' [] = " )"
    prettyPrinterArgumentDeclSet' [a] = (prettyPrinterArgumentDecl a) ++ " )"
    prettyPrinterArgumentDeclSet' (a : as) = (prettyPrinterArgumentDecl a) ++ ", " ++ (prettyPrinterArgumentDeclSet' as)

prettyPrinterFunctionDecl :: FunctionDecl -> String
prettyPrinterFunctionDecl (FunctionDecl t m ptxpi s) = "function " ++ m ++ " " ++ (prettyPrinterArgumentDeclSet ptxpi) ++ " -> " ++ (prettyPrinterType t) ++ (prettyPrinterStatements s)

prettyPrinterFunctionDeclSet :: [FunctionDecl] -> String
prettyPrinterFunctionDeclSet [] = ""
prettyPrinterFunctionDeclSet (m : ms) = (prettyPrinterFunctionDecl m) ++ "\n" ++ (prettyPrinterFunctionDeclSet ms)

prettyPrinterMethodDecl :: MethodDecl -> String
prettyPrinterMethodDecl (MethodDecl t m ptxpi s) = "function " ++ m ++ " " ++ (prettyPrinterArgumentDeclSet ptxpi) ++ " -> " ++ (prettyPrinterType t) ++ (prettyPrinterStatements s)

prettyPrinterMethodDeclSet :: [MethodDecl] -> String
prettyPrinterMethodDeclSet [] = ""
prettyPrinterMethodDeclSet (m : ms) = (prettyPrinterMethodDecl m) ++ "\n" ++ (prettyPrinterMethodDeclSet ms)

prettyPrinterConstructorDecl :: ConstructorDecl -> String
prettyPrinterConstructorDecl (ConstructorDecl m ptxpi s) = "function " ++ m ++ " " ++ (prettyPrinterArgumentDeclSet ptxpi) ++ (prettyPrinterStatements s)

prettyPrinterFunctionType :: FunctionType -> String
prettyPrinterFunctionType (FFunctionType ty0 pi fu1) =
  "(" ++ (prettyPrinterType ty0) ++ "  => " ++ prettyPrinterPurposeSet pi ++ ") -> " ++ (prettyPrinterFunctionType fu1)
prettyPrinterFunctionType (TFunctionType ty0 pi ty1) =
  "(" ++ (prettyPrinterType ty0) ++ "  => " ++ prettyPrinterPurposeSet pi ++ ") -> " ++ (prettyPrinterType ty1)

prettyPrinterMethodType :: MethodType -> String
prettyPrinterMethodType (TMethodType (Purpose p) m) =
  "∀" ++ p ++ ". " ++ (prettyPrinterMethodType m)
prettyPrinterMethodType (FMethodType f) = prettyPrinterFunctionType f

prettyPrinterGroundField :: GroundField -> String
prettyPrinterGroundField (GroundField g f) = prettyPrinterGroundType g ++ " " ++ f ++ ";"

prettyPrinterGroundFieldSet :: [GroundField] -> String
prettyPrinterGroundFieldSet gfs = prettyPrinterGroundFieldSet' gfs
  where
    prettyPrinterGroundFieldSet' [] = ""
    prettyPrinterGroundFieldSet' (gf : fgs) = tab ++ prettyPrinterGroundField gf ++ "\n" ++ prettyPrinterGroundFieldSet' fgs

prettyPrinterClassDecl :: ClassDecl -> String
prettyPrinterClassDecl (ClassDecl c e gt k m) =
  "class "
    ++ c
    ++ " extends "
    ++ e
    ++ " {\n"
    ++ (prettyPrinterGroundFieldSet gt)
    ++ (prettyPrinterConstructorDecl k)
    ++ (prettyPrinterMethodDeclSet m)
    ++ "\n}"

prettyPrinterProgram :: Program -> String
prettyPrinterProgram (Program classes) = prettyPrinterProgram' classes
  where
    prettyPrinterProgram' [] = ""
    prettyPrinterProgram' (c : cs) = prettyPrinterClassDecl c ++ "\n\n" ++ prettyPrinterProgram' cs

main :: IO ()
main = do
  let inpPurposeSet = "{| p0, p2 | RHOOOO |}"
  let inpPurposeSet' = parse parsePurposeSet "" inpPurposeSet
  case inpPurposeSet' of
    Left err -> print err
    Right ast -> do
      print ast
      let prettyPurposeSet = prettyPrinterPurposeSet ast
      putStrLn prettyPurposeSet
  putStrLn ""

  let inpTrueTerm = "true"
  let inpTrueTerm' = parse parseBoolTerm "" inpTrueTerm
  case inpTrueTerm' of
    Left err -> print err
    Right ast -> do
      print ast
      let prettyTermTrue = prettyPrinterTerm ast
      putStrLn prettyTermTrue
  putStrLn ""

  let inpFalseTerm = "false"
  let inpFalseTerm' = parse parseBoolTerm "" inpFalseTerm
  case inpFalseTerm' of
    Left err -> print err
    Right ast -> do
      print ast
      let prettyTermFalse = prettyPrinterTerm ast
      putStrLn prettyTermFalse
  putStrLn ""

  let inpVarTerm = "my_variable"
  let inpVarTerm' = parse parseVarTerm "" inpVarTerm
  case inpVarTerm' of
    Left err -> print err
    Right ast -> do
      print ast
      let prettyTermVar = prettyPrinterTerm ast
      putStrLn prettyTermVar
  putStrLn ""

  let inpIntTerm = "456456"
  let inpIntTerm' = parse parseIntegerTerm "" inpIntTerm
  case inpIntTerm' of
    Left err -> print err
    Right ast -> do
      print ast
      let prettyTermInt = prettyPrinterTerm ast
      putStrLn prettyTermInt
  putStrLn ""

  let inpMethodTerm = "foobar(a, b, c)"
  let inpMethodTerm' = parse parseTerm "" inpMethodTerm
  case inpMethodTerm' of
    Left err -> print err
    Right ast -> do
      print ast
      let prettyTermMethod = prettyPrinterTerm ast
      putStrLn prettyTermMethod
  putStrLn ""

  let inpNewTerm = "new {| p0, p1 |} My_Class(abc, def)"
  let inpNewTerm' = parse parseNewTerm "" inpNewTerm
  case inpNewTerm' of
    Left err -> print err
    Right ast -> do
      print ast
      let prettyTermNew = prettyPrinterTerm ast
      putStrLn prettyTermNew
  putStrLn ""

  let inpTrueValue = "true"
  let inpTrueValue' = parse parseBoolValue "" inpTrueValue
  case inpTrueValue' of
    Left err -> print err
    Right ast -> do
      print ast
      let prettyValueTrue = prettyPrinterValue ast
      putStrLn prettyValueTrue
  putStrLn ""

  let inpFalseValue = "false"
  let inpFalseValue' = parse parseBoolValue "" inpFalseValue
  case inpFalseValue' of
    Left err -> print err
    Right ast -> do
      print ast
      let prettyValueFalse = prettyPrinterValue ast
      putStrLn prettyValueFalse
  putStrLn ""

  let inpIntValue = "456456"
  let inpIntValue' = parse parseIntegerValue "" inpIntValue
  case inpIntValue' of
    Left err -> print err
    Right ast -> do
      print ast
      let prettyValueInt = prettyPrinterValue ast
      putStrLn prettyValueInt
  putStrLn ""

  let inpXAssignmentStatement = "foo := true"
  let inpXAssignmentStatement' = parse parseXAssignmentStatements "" inpXAssignmentStatement
  case inpXAssignmentStatement' of
    Left err -> print err
    Right ast -> do
      print ast
      let prettyStatementXAssignment = prettyPrinterStatements ast
      putStrLn prettyStatementXAssignment
  putStrLn ""

  let inpTAssignmentStatements = "t.f.ff := true"
  let inpTAssignmentStatements' = parse parseTAssignmentStatements "" inpTAssignmentStatements
  case inpTAssignmentStatements' of
    Left err -> print err
    Right ast -> do
      print ast
      let prettyStatementTAssignment = prettyPrinterStatements ast
      putStrLn prettyStatementTAssignment
  putStrLn ""

  let inpBlockStatements = "{ x := 123; asd.f.hfg.asd := false; }"
  let inpBlockStatements' = parse parseBlockStatement "" inpBlockStatements
  case inpBlockStatements' of
    Left err -> print err
    Right ast -> do
      print ast
      let prettyStatementBlock = prettyPrinterStatements ast
      putStrLn prettyStatementBlock
  putStrLn ""

  let inpLetStatements = "let x : bool {| Private |} := true in { x := false }"
  let inpLetStatements' = parse parseLetStatements "" inpLetStatements
  case inpLetStatements' of
    Left err -> print err
    Right ast -> do
      print ast
      let prettyStatementLet = prettyPrinterStatements ast
      putStrLn prettyStatementLet
  putStrLn ""

  let inpIfStatements = "if true then { x:= 123; } else { x:= 321; }"
  let inpIfStatements' = parse parseIfStatements "" inpIfStatements
  case inpIfStatements' of
    Left err -> print err
    Right ast -> do
      print ast
      let prettyStatementIf = prettyPrinterStatements ast
      putStrLn prettyStatementIf
  putStrLn ""

  let inpDoWhileStatements = "do { x:= 123; } while ( true )"
  let inpDoWhileStatements' = parse parseDoWhileStatements "" inpDoWhileStatements
  case inpDoWhileStatements' of
    Left err -> print err
    Right ast -> do
      print ast
      let prettyStatementDoWhile = prettyPrinterStatements ast
      putStrLn prettyStatementDoWhile
  putStrLn ""

  let inpWhileStatements = "while ( true ) do { x:= 123; }"
  let inpWhileStatements' = parse parseWhileStatements "" inpWhileStatements
  case inpWhileStatements' of
    Left err -> print err
    Right ast -> do
      print ast
      let prettyStatementWhile = prettyPrinterStatements ast
      putStrLn prettyStatementWhile
  putStrLn ""

  let inpXGrantStatements = "foo.grant(abc)"
  let inpXGrantStatements' = parse parseXGrantStatement "" inpXGrantStatements
  case inpXGrantStatements' of
    Left err -> print err
    Right ast -> do
      print ast
      let prettyStatementXGrant = prettyPrinterStatements ast
      putStrLn prettyStatementXGrant
  putStrLn ""

  let inpXRevokeStatements = "foo.revoke(abc)"
  let inpXRevokeStatements' = parse parseXRevokeStatement "" inpXRevokeStatements
  case inpXRevokeStatements' of
    Left err -> print err
    Right ast -> do
      print ast
      let prettyStatementXRevoke = prettyPrinterStatements ast
      putStrLn prettyStatementXRevoke
  putStrLn ""

  let inpArgumentDeclXintP0 = "x : int {| p0 |}"
  let inpArgumentDeclXintP0' = parse parseArgumentDecl "" inpArgumentDeclXintP0
  case inpArgumentDeclXintP0' of
    Left err -> print err
    Right ast -> do
      print ast
      let prettyArgumentDeclXintP0 = prettyPrinterArgumentDecl ast
      putStrLn prettyArgumentDeclXintP0
  putStrLn ""

  let inpArgumentDeclXintP0Bar = "x : int {| p0 | bar|}"
  let inpArgumentDeclXintP0Bar' = parse parseArgumentDecl "" inpArgumentDeclXintP0Bar
  case inpArgumentDeclXintP0Bar' of
    Left err -> print err
    Right ast -> do
      print ast
      let prettyArgumentDeclXintP0Bar = prettyPrinterArgumentDecl ast
      putStrLn prettyArgumentDeclXintP0Bar
  putStrLn ""

  let inpArgumentDeclXintP0p1Bar = "x : int {| P0, p1 | bar|}"
  let inpArgumentDeclXintP0p1Bar' = parse parseArgumentDecl "" inpArgumentDeclXintP0p1Bar
  case inpArgumentDeclXintP0p1Bar' of
    Left err -> print err
    Right ast -> do
      print ast
      let prettyArgumentDeclXintP0p1Bar = prettyPrinterArgumentDecl ast
      putStrLn prettyArgumentDeclXintP0p1Bar
  putStrLn ""

  let inpFunctionDeclIdentity = "function identity (a : int {| P0 |} => {| P0 |}) -> int {| p0 |} {return a;}"
  let inpFunctionDeclIdentity' = parse parseFunctionDecl "" inpFunctionDeclIdentity
  case inpFunctionDeclIdentity' of
    Left err -> print err
    Right ast -> do
      print ast
      let prettyFunctionDeclIdentity = prettyPrinterFunctionDecl ast
      putStrLn prettyFunctionDeclIdentity
  putStrLn ""

  let inpFunctionDeclIdentityNested = "function nested() -> int {|p3|} {if true then {return true;x := 123;if true then {return true;x := 123;} else {return false;y := 654;};} else {return false;y := 654;};return a;}"
  let inpFunctionDeclIdentityNested' = parse parseFunctionDecl "" inpFunctionDeclIdentityNested
  case inpFunctionDeclIdentityNested' of
    Left err -> print err
    Right ast -> do
      print ast
      let prettyFunctionDeclIdentityNested = prettyPrinterFunctionDecl ast
      putStrLn prettyFunctionDeclIdentityNested
  putStrLn ""

  let inpConstructorfooA = "function foo(a : int {|p0|} => {|p1|} ){a := a;}"
  let inpConstructorfooA' = parse (parseConstructor "foo") "" inpConstructorfooA
  case inpConstructorfooA' of
    Left err -> print err
    Right ast -> do
      print ast
      let prettyConstructorFooA = prettyPrinterConstructorDecl ast
      putStrLn prettyConstructorFooA
  putStrLn ""

  let classFooBar = "class foo extends bar { int a; function foo(a : int {|p0|} => {|p1|} ){ a := a; } function aGetter() -> int {|p3|} { return a; } }"
  let classFooBar' = parse parseClassDecl "" classFooBar
  case classFooBar' of
    Left err -> print err
    Right ast -> do
      print ast
      let prettyClassFooBar = prettyPrinterClassDecl ast
      putStrLn prettyClassFooBar
  putStrLn ""

  putStrLn ""