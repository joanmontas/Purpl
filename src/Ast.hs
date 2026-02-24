module Ast where

newtype Purpose = Purpose String
  deriving (Show, Eq)

-- G ::= Bool | Int | String | C | · · ·
data GroundType
  = BoolGroundType
  | IntGroundType
  | StringGroundType
  | ClassGroundType String
  | UnitGroundType -- When no types is given aiding in giving function type signature
  deriving (Show, Eq)

-- π ::= {p1, p2, ..., pn} | {| p1, ..., pn | ρ |}
data PurposeSet
  = PurposeSet {purposes :: [String], rho :: Maybe String}
  | AnyPurpose -- This represents Any Purpose
  deriving (Show, Eq)

-- T ::= G π
data Type = Type
  { gtType :: GroundType,
    piType :: PurposeSet
  }
  deriving (Show, Eq)

-- t ::= x | t.f | t.m(x) | new_π C(ŧ) | true | false
data Term
  = VarTerm String
  | FieldAccessTerm {sTerm :: Term, fTerm :: String}
  | MethodCallTerm {sTerm :: Term, mTerm :: String, argTerm :: [Term]}
  | NewTerm {piTerm :: PurposeSet, cTerm :: String, argsTerm :: [Term]}
  | TrueTerm
  | FalseTerm
  | IntegerTerm {nTerm :: Integer}
  | StringTerm {strTerm :: String}
  deriving (Show, Eq)

-- v ::= new_π C(ṽ) | true | false
data Value
  = NewValue {piValue :: PurposeSet, cValue :: String, argValue :: [Value]}
  | TrueValue
  | FalseValue
  | IntegerValue Integer
  | StringValue String
  deriving (Show, Eq)

-- s ::= x ..= t | t.f ..= t | s; s | skip
-- -- | let x : T ..= t in s
-- -- | if t then s else s
-- -- | do s while t | while t do s
-- -- | x.grant(p)
-- -- | x.revoke(p)
data Statements
  = XAssignmentStatements {xStatements :: String, tStatements :: Term}
  | TAssignmentStatements {t_fStatements :: Term, tStatements :: Term}
  | SkipStatements
  | BlockStatements {bStatements :: [Statements]}
  | LetStatements {xStatements :: String, tyStatements :: Type, tStatements :: Term, sStatements :: Statements}
  | IfStatements {tStatements :: Term, s0Statements :: Statements, s1Statements :: Statements}
  | DoStatements {sStatement :: Statements, tStatements :: Term}
  | WhileStatements {tStatements :: Term, sStatement :: Statements}
  | MethodCallStatements {tMethodCall :: Term} -- invocation
  | XGrantStatements {xStatements :: String, pStatements :: String}
  | XRevokeStatements {xStatements :: String, pStatements :: String}
  | ReturnStatements {tStatements :: Term}
  deriving (Show, Eq)

data TypeVar = TypeVar
  { tyTypeVar :: Type,
    xTypeVar :: String
  }
  deriving (Show, Eq)

-- M ::= ∀ρ. M | F
data MethodType
  = TMethodType {pMethodType :: Purpose, mMethodType :: MethodType}
  | FMethodType {fMethodType :: FunctionType}
  deriving (Show, Eq)

-- F ::= (T ⇒ π) → F | (T ⇒ π) → T
data FunctionType
  = FFunctionType {ty0FunctionType :: Type, piFunctionType :: PurposeSet, fu1FunctionType :: FunctionType} -- nested arguments
  | TFunctionType {ty0FunctionType :: Type, piFunctionType :: PurposeSet, ty1FunctionType :: Type} -- terminal arg, the return type is defined here
  deriving (Show, Eq)

data ArgumentDecl = ArgumentDecl
  { nArgumentDecl :: String,
    tArgumentDecl :: Type, -- (GroundType,PurposeSet)
    piArgumentDecl :: PurposeSet -- Side Effect/Transition
  }
  deriving (Show, Eq)

data FunctionDecl = FunctionDecl
  { tFunctionDecl :: Type,
    mFunctionDecl :: String,
    ptxpiFunctionDecl :: [ArgumentDecl],
    sFunctionDecl :: Statements
  }
  deriving (Show, Eq)

data ConstructorDecl = ConstructorDecl
  { mConstructorDecl :: String,
    ptxpiConstructorDecl :: [ArgumentDecl],
    sConstructorDecl :: Statements
  }
  deriving (Show, Eq)

-- M ::= T m(∀ρ.T x ⇒ π){s}
data MethodDecl = MethodDecl
  { tMethodDecl :: Type,
    mMethodDecl :: String,
    ptxpiMethodDecl :: [ArgumentDecl],
    sMethodDecl :: Statements
  }
  deriving (Show, Eq)

data GroundField = GroundField
  { gGroundField :: GroundType,
    fGroundField :: String
  }
  deriving (Show, Eq)

-- CL ::= class C extends C{Gf , M }
data ClassDecl = ClassDecl
  { cClassDecl :: String,
    extendsClassDecl :: String,
    gtClassDecl :: [GroundField],
    kClassDecl :: ConstructorDecl,
    mClassDecl :: [MethodDecl]
  }
  deriving (Show, Eq)

newtype Program = Program [ClassDecl]
  deriving (Show, Eq)

data Ast
  = PurposeSetAst PurposeSet
  | ProgramAst Program
  | ClassDeclAst ClassDecl
  | FunctionDeclAst FunctionDecl
  | MethodDeclAst MethodDecl
  | ConstructorDeclAst ConstructorDecl
  | StatementsAst Statements
  | TermAst Term
  | ValueAst Value
  | GroundTypeAst GroundType
  | TypeAst Type
  deriving (Show, Eq)
