module TypeCheckerTypes where

import Data.Map qualified as Map
import Ast

-- Description:
-- -- Instead of having a recursive/curried function denoting Method Type
-- -- We will flatten it for easier processing method signature
data FlatMethodType = FlatMethodType
  { allRhosFlattenMethodType :: [Maybe String], -- combined rho's arguments
    argFlattenMethodType :: [ArgumentDecl], -- name type pair string + Type
    retFlattenMethodType :: Type -- GrounType + PurposeSet
  }
  deriving (Show, Eq)

-- -- -- -- Maps Types -- -- --

-- Map { method_name :: String -> method_ast :: MethodDecl }
type Class_Table_Method_Present = Map.Map String MethodDecl

-- Map { method_name :: String -> method_type :: MethodType }
type Class_Table_Method_Parent = Map.Map String MethodType

-- Map {argument_name :: String -> flat_method_type :: FlatMethodType}
type Class_Table_Method_Flat_Parent = Map.Map String FlatMethodType

-- Map { class_field :: String -> field_type :: GroundType }
type Class_Table_Field_Type = Map.Map String GroundType -- TODO(Joan) Come back later - Joan

-- Class name to MethodInfo
data ClassInfo = ClassInfo
  { extendsClassInfo :: String, -- extends class
    fieldTypeClassInfo :: Class_Table_Field_Type, -- field-name to GroundType
    methodPresentClassInfo :: Class_Table_Method_Present, -- method-name to ClassDecl an AST
    methodTypeClassInfo :: Class_Table_Method_Parent, -- method-name to method-type
    flatMethodTypeClassInfo :: Class_Table_Method_Flat_Parent, -- method-name to flatten method-type
    constructorFlatTypeClassInfo :: FlatMethodType,
    classPurposeSetClassInfo :: PurposeSet -- union of class constructors's argument's purposes
  }
  deriving (Show, Eq)

type ClassTable = Map.Map String ClassInfo

type PMatches = Map.Map String [PurposeSet]

type Gamma = Map.Map String Type

type PEnv = Map.Map String State

-- Description: List of rho variable in the environment
-- -- When we are 'checking' ad we encounter rho0,
-- -- then rho0 must be in the Delta/scope
type Delta = [Maybe String]

-- -- -- Return Types -- -- --
type CheckResult a = Either String (a, Gamma, PEnv)

type CheckStatementResult = Either String (Gamma, PEnv)

type CheckMethodDeclResult = Either String ()

-- -- -- Initializations -- -- --

-- for now Internal, but eventually empty will valid
internal :: String
internal = "Internal"

internalPurposeSet :: PurposeSet
internalPurposeSet = PurposeSet ["Internal"] Nothing

emptyPurposeSet :: PurposeSet
emptyPurposeSet = PurposeSet [] Nothing

-- Will remove later
defaultPurposeSet :: Type -> Type
defaultPurposeSet (Type ground (PurposeSet [] Nothing)) = Type ground internalPurposeSet
defaultPurposeSet other = other