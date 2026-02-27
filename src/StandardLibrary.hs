module StandardLibrary where

import qualified Data.Map as Map
import Ast
import TypeCheckerTypes

-- Description: For now, this is our Standard Library.
-- -- All classes extends from this "Object" class.
defaultObjectInfo :: ClassInfo
defaultObjectInfo =
  ClassInfo
    { extendsClassInfo = "",
      fieldTypeClassInfo = Map.fromList [("ObjectID", IntGroundType)],
      methodPresentClassInfo = Map.fromList [("getObjectID", objectMethodAST)],
      methodTypeClassInfo = Map.fromList [("getObjectID", objectMethodType)],
      flatMethodTypeClassInfo = Map.fromList [("getObjectID", objectFlatMethodType)],
      constructorFlatTypeClassInfo = objectConstructorFlatType,
      classPurposeSetClassInfo = PurposeSet {purposes = [], rho = Nothing}
    }
  where
    objectMethodType =
      FMethodType
        { fMethodType =
            TFunctionType
              { ty0FunctionType =
                  Type
                    { gtType = UnitGroundType,
                      piType = PurposeSet {purposes = [], rho = Nothing}
                    },
                piFunctionType = PurposeSet {purposes = [], rho = Nothing},
                ty1FunctionType =
                  Type
                    { gtType = IntGroundType,
                      piType = PurposeSet {purposes = [], rho = Nothing}
                    }
              }
        }

    objectFlatMethodType =
      FlatMethodType
        { allRhosFlattenMethodType = [],
          argFlattenMethodType = [],
          retFlattenMethodType =
            Type
              { gtType = IntGroundType,
                piType = PurposeSet {purposes = [], rho = Nothing}
              }
        }

    objectMethodAST =
      MethodDecl
        { tMethodDecl =
            Type
              { gtType = IntGroundType,
                piType = PurposeSet {purposes = [], rho = Nothing}
              },
          mMethodDecl = "getObjectID",
          ptxpiMethodDecl = [],
          sMethodDecl =
            BlockStatements {bStatements = [ReturnStatements {tStatements = VarTerm "ObjectID"}]},
          sPurposeState = Nothing,
          fPurposeState = Nothing
        }

    objectConstructorFlatType =
      FlatMethodType
        { allRhosFlattenMethodType = [],
          argFlattenMethodType = [],
          retFlattenMethodType = Type (ClassGroundType "Object") AnyPurpose
        }

-- Description: Creates a ClassTable containing the 'Top' Object Class.
initialClassTable :: ClassTable
initialClassTable = Map.singleton "Object" defaultObjectInfo