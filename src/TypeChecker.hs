module TypeChecker where
import Ast
import TypeCheckerTypes
import StandardLibrary
import TypeCheckerUtils
-- import Parser (parseProgram)
import Data.List ((\\))
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Debug.Trace (trace) -- NOTE(Joan) For DEBUG Remove later - Joan
import Parser
import PrettyPrinter
import Text.Parsec (parse)

-- -- -- -- -- Phase 1 -- -- -- -- --

-- Description: Creates ClassTable given a program. Moreover, the ClassInfo inherits from its base class.
classTableConstructor :: Program -> ClassTable
classTableConstructor (Program classes) = classTableConstructor' classes (initialClassTable)
  where
    classTableConstructor' [] ct = ct
    classTableConstructor' (c : cs) ct =
      let classInfo = classInfoConstructor c
          className = cClassDecl c
          -- TODO(Joan) Make sure it is an unique name - Joan
          extendName = extendsClassDecl c
          inherited =
            case Map.lookup extendName ct of
              Nothing -> classInfo
              Just baseInfo ->
                classInfo
                  { fieldTypeClassInfo = Map.union (fieldTypeClassInfo classInfo) (fieldTypeClassInfo baseInfo),
                    methodTypeClassInfo = Map.union (methodTypeClassInfo classInfo) (methodTypeClassInfo baseInfo),
                    flatMethodTypeClassInfo = Map.union (flatMethodTypeClassInfo classInfo) (flatMethodTypeClassInfo baseInfo),
                    classPurposeSetClassInfo = PurposeSet {purposes = (List.nub (purposes (classPurposeSetClassInfo classInfo) ++ purposes (classPurposeSetClassInfo baseInfo))), rho = Nothing}
                  }
       in classTableConstructor' cs (Map.insert (cClassDecl c) inherited ct)

    -- ClassDecl -> ClassInfo
    classInfoConstructor c =
      let className = cClassDecl c
          k = kClassDecl c
          ms = mClassDecl c
          fs = gtClassDecl c

          pSignatures = concatMap (\m -> extract_purposes_from_ArgumentDecls (ptxpiMethodDecl m) [] ++ purposes (piType (tMethodDecl m))) ms
          pKArgs = extract_purposes_from_ArgumentDecls (ptxpiConstructorDecl k) []

          allPurposes = List.nub (pSignatures ++ pKArgs)
          classPi = PurposeSet allPurposes Nothing

          kRhos = Set.toList (Set.fromList (extract_rhos_from_ArgumentDecls (ptxpiConstructorDecl k) []))
          flatK =
            FlatMethodType
              { allRhosFlattenMethodType = map Just kRhos,
                argFlattenMethodType = ptxpiConstructorDecl k,
                retFlattenMethodType = Type (ClassGroundType className) classPi
              }
       in ClassInfo
            { extendsClassInfo = extendsClassDecl c,
              fieldTypeClassInfo = class_Table_Field_Type_constructor fs Map.empty,
              methodPresentClassInfo = class_Table_Method_Present_constructor ms Map.empty,
              methodTypeClassInfo = class_Table_Method_Parent_constructor ms Map.empty,
              flatMethodTypeClassInfo = class_Table_Method_Flat_Parent_constructor ms Map.empty,
              constructorFlatTypeClassInfo = flatK,
              classPurposeSetClassInfo = classPi
            }

    -- [GroundField] -> Class_Table_Field_Type -> Class_Table_Field_Type
    class_Table_Field_Type_constructor [] cTFT = cTFT
    class_Table_Field_Type_constructor (gf : gfs) cTFT =
      class_Table_Field_Type_constructor gfs (Map.insert (fGroundField gf) (gGroundField gf) cTFT)

    -- [MethodDecl] -> Class_Table_Method_Present -> Class_Table_Method_Present
    class_Table_Method_Present_constructor [] cTMP = cTMP
    class_Table_Method_Present_constructor (m : ms) cTMP =
      class_Table_Method_Present_constructor ms (Map.insert (mMethodDecl m) m cTMP)

    -- Signature: [MethodDecl] -> Class_Table_Method_Parent -> Class_Table_Method_Parent
    -- Description: Creates a map from method-name to MethodType
    -- Input:
    -- -- [MethodDecl] all methods who will be interrogated
    -- -- Initially empty map containing method-name to MethodType
    -- Output Returns a map containing method-name to MethodType
    class_Table_Method_Parent_constructor [] cTMPr = cTMPr
    class_Table_Method_Parent_constructor (m : ms) cTMPr =
      let mT = class_Table_Method_Parent_constructor_FMethodType m
       in class_Table_Method_Parent_constructor ms (Map.insert (mMethodDecl m) mT cTMPr)

    -- Signature: MethodDecl ->  MethodType
    -- Description: Takes a MethodDecl and returns its type signature
    -- Inputs: MethodDecl whose argument and return type will be interrogated
    -- Output: MethodType
    class_Table_Method_Parent_constructor_FMethodType m =
      let fMT = class_Table_Method_Parent_constructor_FMethodType' (ptxpiMethodDecl m) (tMethodDecl m)
          rho = Set.toList (Set.fromList (extract_rhos_from_ArgumentDecls (ptxpiMethodDecl m) [])) -- NOTE(Joan) Come back to this - Joan
       in class_Table_Method_Parent_constructor_TMethodType FMethodType {fMethodType = fMT} rho

    -- Signature: [ArgumentDecl] -> Type -> FMethodType
    -- Description: Takes list of argument and method-return-types.
    --  -- Extracting and nesting argument types final MethodType contains return type.
    -- Inputs:
    -- -- (arg:args) : [Arguments] list of argument
    -- -- rt : Type Representing the return type to be added at terminal TFunctionType
    -- Outputs:
    -- -- fMT : FunctionType containing(nested) Method arguments types and return type
    class_Table_Method_Parent_constructor_FMethodType' [] rt =
      TFunctionType
        { ty0FunctionType = Type UnitGroundType (PurposeSet [] Nothing),
          piFunctionType = PurposeSet [] Nothing,
          ty1FunctionType = rt
        }
    class_Table_Method_Parent_constructor_FMethodType' [arg] rt =
      TFunctionType
        { ty0FunctionType = tArgumentDecl arg,
          piFunctionType = piArgumentDecl arg,
          ty1FunctionType = rt
        }
    class_Table_Method_Parent_constructor_FMethodType' (arg : args) rt =
      FFunctionType
        { ty0FunctionType = tArgumentDecl arg,
          piFunctionType = piArgumentDecl arg,
          fu1FunctionType = class_Table_Method_Parent_constructor_FMethodType' args rt
        }

    -- Signature: MethodType -> [String] -> FunctionType
    -- Description: Takes an FMethodType and wraps it with TMethodType per every rho.
    -- Inputs:
    -- -- mt a MethodType/FMethodType
    -- -- (rho:rhos), an array of strings representing each rho.
    -- Outputs: FunctionType. TMethodType when rhos are found otherwise returns the given FMethodType.
    class_Table_Method_Parent_constructor_TMethodType mt [] = mt
    class_Table_Method_Parent_constructor_TMethodType mt (rho : rhos) =
      TMethodType
        { pMethodType = Purpose rho, -- Purpose
          mMethodType = class_Table_Method_Parent_constructor_TMethodType mt rhos
        }

    -- Signature: [MethodDecl] -> Class_Table_Method_Flat_Parent -> Class_Table_Method_Flat_Parent
    -- Description: Takes a MethodDecl and returns its type signature, not as a recursive type, but flat.f
    class_Table_Method_Flat_Parent_constructor [] cTMFP = cTMFP
    class_Table_Method_Flat_Parent_constructor (m : ms) cTMFP =
      let rhos = Set.toList (Set.fromList (extract_rhos_from_ArgumentDecls (ptxpiMethodDecl m) []))

          flatMT =
            FlatMethodType
              { allRhosFlattenMethodType = map Just rhos,
                argFlattenMethodType = ptxpiMethodDecl m,
                retFlattenMethodType = tMethodDecl m
              }
       in class_Table_Method_Flat_Parent_constructor ms (Map.insert (mMethodDecl m) flatMT cTMFP)


-- -- -- -- -- Phase 2   -- -- -- -- --

checkConstructorArgs :: ClassTable -> Gamma -> PEnv -> Delta -> [ArgumentDecl] -> [Term] -> Either String (Gamma, PEnv)
checkConstructorArgs _ gamma pEnv _ [] [] = Right (gamma, pEnv)
checkConstructorArgs ct gamma pEnv delta (argDecl : args) (tTerm : ts) = do
  (Type gt_actual pi_actual, gamma', pEnv') <- checkTerm ct gamma pEnv delta tTerm
  let t_expected = tArgumentDecl argDecl
  if isSubType ct gt_actual (gtType t_expected)
    then
      if isSubPurpose pi_actual (piType t_expected)
        then
          checkConstructorArgs ct gamma' pEnv' delta args ts
        else
          let missing =
                Set.toList
                  ( Set.fromList (purposes (piType t_expected))
                      `Set.difference` Set.fromList (purposes pi_actual)
                  )
           in Left $
                "ERROR Purpose Mismatch: checkConstructorArgs. Missing purposes: "
                  ++ show missing
                  ++ ". (Expected "
                  ++ show (piType t_expected)
                  ++ " but got "
                  ++ show pi_actual
                  ++ ")"
    else
      Left $
        "ERROR Type Mismatch: checkConstructorArgs. Expected "
          ++ show (gtType t_expected)
          ++ " but got "
          ++ show gt_actual
checkConstructorArgs _ _ _ _ _ _ = Left "ERROR: checkConstructorArgs Constructor Argument Count Mismatch"

-- Description: Given a term returns its Type given Gamma
checkTerm :: ClassTable -> Gamma -> PEnv -> Delta -> Term -> CheckResult Type
checkTerm ct gamma pEnv delta TrueTerm =
  Right (Type BoolGroundType AnyPurpose, gamma, pEnv)
checkTerm ct gamma pEnv delta FalseTerm =
  Right (Type BoolGroundType AnyPurpose, gamma, pEnv)
checkTerm ct gamma pEnv delta (IntegerTerm _) =
  Right (Type IntGroundType AnyPurpose, gamma, pEnv)
checkTerm ct gamma pEnv delta (StringTerm _) =
  Right (Type StringGroundType AnyPurpose, gamma, pEnv)
checkTerm ct gamma pEnv delta (VarTerm x) =
  case Map.lookup x gamma of
    Nothing -> Left ("ERROR: checkTerm -> VarTerm the variable " ++ x ++ " is out of scope")
    Just ty -> Right (ty, gamma, pEnv)
checkTerm ct gamma pEnv delta (NewTerm concretePurposes className argTerms) = do
  classInfo <- case Map.lookup className ct of
    Just info -> Right info
    Nothing -> Left $ "ERROR: checkTerm -> NewTerm Unknown class " ++ className
  let kFlat = constructorFlatTypeClassInfo classInfo
  let kArgs = argFlattenMethodType kFlat
  if length kArgs /= length argTerms
    then Left $ "ERROR: checkTerm -> NewTerm Constructor mismatch in " ++ className
    else do
      (gamma', pEnv') <- checkConstructorArgs ct gamma pEnv delta kArgs argTerms
      return (Type (ClassGroundType className) concretePurposes, gamma', pEnv')
-- Inference:
-- -- Γ ⊢ t : C_t π_t ▷ Γ′           Γ′i ⊢ x_i : G_x,_i  π_x,_i ▷ Γ′′_i
-- -- Γ′_i = Γ′′_i−1
-- -- Γ′_1 = Γ′
-- -- CT(C_t, m) = ∀ρ_x,_i. (Gx,iπ′x,i => π′′_x,_i) -> G_π
-- -- σ = Union_xi (π′_x,_i, π_x,_i)
-- -- is-fn(σ)
-----------------------------------------------------------------------
-- -- -- Γ ⊢ t.m(x1 . . . xn) : G(πσ) ▷ Γ′′n[xi 7 → Gx,i(π′′x,iσ)]
checkTerm ct gamma pEnv delta (MethodCallTerm s m arg) = do
  -- Γ ⊢ t : C_t π_t ▷ Γ′
  (t_t, gamma', pEnv') <- checkTerm ct gamma pEnv delta s

  case gtType t_t of
    ClassGroundType className -> do
      classInfo <- case Map.lookup className ct of
        Just info -> Right info
        Nothing -> Left ("ERROR: checkTerm -> MethodCallTerm " ++ className ++ " given is not found inside the Class Table")

      -- TODO() perform state condition check (Pre-condition)
      mtdDecl <- case Map.lookup m (methodPresentClassInfo classInfo) of
        Just mtd -> Right mtd
        Nothing -> Left ("ERROR: checkTerm -> MethodCallTerm " ++ className ++ " given is not found inside the Class Table")

      case sPurposeState mtdDecl of
        Nothing -> Right () -- no state transition expected
        Just (PurposeState ps expected_transition) ->
          mapM_ (checkStateTransition pEnv' expected_transition) ps -- perform transition

      -- CT(C_t, m) = ∀ρ_x,_i. (Gx,iπ′x,i => π′′_x,_i) -> G_π
      flatMethodSignature <- case Map.lookup m (flatMethodTypeClassInfo classInfo) of
        Just signature -> Right signature
        Nothing -> Left ("ERROR: checkTerm -> MethodCallTerm the m given is not found ")

      let argFlat = argFlattenMethodType flatMethodSignature

      if length argFlat /= length arg
        then Left ("ERROR: checkTerm -> MethodCallTerm expected " ++ show (length argFlat) ++ " arguments but got " ++ show (length arg))
        else do
          --  Γ′_1 = Γ′
          (gamma_n, pEnv_n, sigma) <- bigUnionArgs ct gamma' pEnv' delta Map.empty argFlat arg

          -- TODO() Remove DEBUG
          let gammaDump =
                "\n>>>>> TRACE Method: "
                  ++ m
                  ++ "\n"
                  ++ unlines ["      " ++ k ++ " : " ++ show v | (k, v) <- Map.toList gamma_n]

          if is_fn sigma
            then Left "ERROR: checkTerm -> MethodCallTerm is-fn(σ) resulted in conflicting purpose sets."
            else do
              -- Γ ⊢ t.m(x1 . . . xn) : G(πσ)

              -- TODO() perform state condition transition (Post-condition)
              let pEnv_final = case fPurposeState mtdDecl of
                    Nothing -> pEnv_n
                    Just (PurposeState ps target_transition) ->
                      foldl (\acc p -> Map.insert p target_transition acc) pEnv_n ps -- performTransition

              -- TODO() Remove DEBUG
              trace gammaDump $ do
                let retTy = retFlattenMethodType flatMethodSignature
                -- Return the updated pEnv_n along with the result
                return (retTy, gamma_n, pEnv_final)
    -- TODO() Uncomment and Remove the DEBUGs above
    -- let retTy = retFlattenMethodType flatMethodSignature
    -- return (retTy, gamma_n, pEnv_n)
    _ -> Left ("ERROR: checkTerm -> MethodCallTerm the t (Term) given is not of type Class")
  where
    -- Description: Recursively processes method arguments to solve for Rho (sigma) and
    bigUnionArgs ct gamma pEnv delta total_pMatches [] [] = Right (gamma, pEnv, total_pMatches)
    bigUnionArgs ct gamma pEnv delta total_pMatches (argDecl : args) (t : ts) = do
      -- Γ′i ⊢ x_i : G_x,_i π_x,_i ▷ Γ′′_i
      (t_actual, gamma_double_prime, pEnv_double_prime) <- checkTerm ct gamma pEnv delta t

      if gtType t_actual /= gtType (tArgumentDecl argDecl)
        then
          Left
            ( "ERROR Type Mismatch: checkTerm -> MethodCallTerm -> bigUnionArgs Type Mismatch."
                ++ "Expected "
                ++ (show (gtType t_actual))
                ++ " but got "
                ++ (show (gtType (tArgumentDecl argDecl)))
            )
        else do
          let expected_pi = piType (tArgumentDecl argDecl)
          let method_arg_pi = piType t_actual
          let missing = Set.toList (Set.fromList (purposes expected_pi) `Set.difference` Set.fromList (purposes method_arg_pi))

          if not (null missing)
            then Left ("ERROR Purpose Misuse: checkTerm -> MethodCallTerm -> bigUnionArgs: Missing required purposes: " ++ show missing)
            else do
              next_total <- pMatch total_pMatches expected_pi method_arg_pi

              -- Description: Removes revoked Purposea, add what is granted and leave what was not touched.
              -- In doing so, assures transition of arguments during call.
              let next_gamma = case t of
                    VarTerm varName ->
                      let requirementPi = piType (tArgumentDecl argDecl)
                          targetPi = piArgumentDecl argDecl
                       in if requirementPi == targetPi
                            then gamma_double_prime
                            else
                              let currentPi = method_arg_pi -- The piType we got from checkTerm
                                  revoked = purposes requirementPi
                                  granted = purposes targetPi
                                  persisted = purposes currentPi List.\\ revoked
                                  newPurposes = List.nub (granted ++ persisted)
                                  newPi = currentPi {purposes = newPurposes, rho = getRho targetPi}
                                  newTy = Type (gtType t_actual) newPi
                               in Map.insert varName newTy gamma_double_prime
                    _ -> gamma_double_prime

              -- Pass the updated pEnv_double_prime to the next argument
              bigUnionArgs ct next_gamma pEnv_double_prime delta next_total args ts

    checkStateTransition env expected p =
      case Map.lookup p env of
        Just actual | actual == expected -> Right ()
        Just actual ->
          Left $
            "ERROR: checkTerm -> MethodCallTerm  -> checkStateTransition -> Final Transition Failed: Purpose "
              ++ p
              ++ " ended in "
              ++ show actual
              ++ " but expected "
              ++ show expected
        Nothing -> do
          if ActiveState == expected
            then Right ()
            else
              Left $
                "ERROR: checkTerm -> MethodCallTerm  -> checkStateTransition -> Final Transition Failed: Purpose "
                  ++ p
                  ++ " ended in "
                  ++ (show ActiveState)
                  ++ " but expected "
                  ++ show expected

-- Inference:
-- -- Γ ⊢ t : C_tπ_t ▷ Γ'
-- -- CT(Ct, f) = G
------------------------
-- -- Γ ⊢ t.f : Gπt ▷ Γ′
checkTerm ct gamma pEnv delta (FieldAccessTerm s f) = do
  -- Γ ⊢ t : C_tπ_t ▷ Γ', pEnv'
  (t_t, gamma', pEnv') <- checkTerm ct gamma pEnv delta s
  case gtType t_t of
    ClassGroundType className ->
      case Map.lookup className ct of
        Just info ->
          case Map.lookup f (fieldTypeClassInfo info) of -- CT(Ct, f) = G
            Just fieldGroundType ->
              -- Γ ⊢ t.f : Gπt ▷ Γ′, pEnv'
              return (Type fieldGroundType (piType t_t), gamma', pEnv')
            Nothing ->
              Left $ "ERROR: Field '" ++ f ++ "' not found in class '" ++ className ++ "'"
        Nothing ->
          Left $ "ERROR: Class '" ++ className ++ "' not found in Class Table"
    _ ->
      Left $
        "ERROR Type Mismatch: Cannot access field '"
          ++ f
          ++ "' on a non-object type: "
          ++ show (gtType t_t)

-- Description: Given a Statement make sure there are no Misuse of Purposes
-- Mistypes and return the updated Gamma.
checkStatement :: ClassTable -> Gamma -> PEnv -> Delta -> Statements -> CheckStatementResult
checkStatement _ gamma pEnv _ SkipStatements =
  Right (gamma, pEnv)
-- Description:
-- Syntax: let x : T ..= t in s
-- -- x : Name of let variable
-- -- T : Type of the x variable
-- -- t : terms being assigned to the x variable
-- -- s : Scope of the let-statement
-- Inference:
-- -- CT; C; Γ; ∆ ⊢ t : T_t ▷ Γ′     T_x ⊆ T_t    T_x == G_xπ_x            ∆ ⊢ π_x
-- -- CT; C; Γ′[x → T_x]; ∆ ⊢ s ▷ Γ′′
-- -- ----------------------------------------------------------------------------
-- -- CT; C; Γ; ∆ ⊢ let x : Tx = t in s ▷ (Γ′′ \ x)[x → Γ′(x)]
checkStatement ct gamma pEnv delta (LetStatements x t_x t s) = do
  (t_t, gamma', pEnv') <- checkTerm ct gamma pEnv delta t -- CT; C; Γ; ∆ ⊢ t : T_t ▷ Γ′, pEnv′
  -- let delta' = (getRho (piType t_t)) : delta -- ∆ ⊢ π_x -- Come back
  if (isSubType ct (gtType t_t) (gtType t_x))
    then
      if (isSubPurpose (piType t_t) (piType t_x))
        then do
          let gamma_with_x = Map.insert x t_x gamma' -- Γ′[x → T_x]
          (gamma_after_s, pEnv'') <- checkStatement ct gamma_with_x pEnv' delta s -- CT; C; Γ′[x → T_x]; ∆ ⊢ s ▷ Γ′′, pEnv′′
          let gamma'' = case Map.lookup x gamma' of -- (Γ′′ \ x)[x → Γ′(x)]
                Just _ -> gamma_after_s
                Nothing -> Map.delete x gamma_after_s
          return (gamma'', pEnv'') -- CT; C; Γ; ∆ ⊢ let x : Tx = t in s ▷ (Γ′′ \ x)[x → Γ′(x)], pEnv′′
        else
          let expected_pi = piType t_x
              actual_pi = piType t_t
              missing =
                Set.toList
                  ( Set.fromList (purposes expected_pi)
                      `Set.difference` Set.fromList (purposes actual_pi)
                  )
           in Left $
                "ERROR Purpose Misuse: CheckStatement -> LetStatement of variable "
                  ++ x
                  ++ ". Missing purposes: "
                  ++ show missing
                  ++ " (Expected "
                  ++ show expected_pi
                  ++ " but got "
                  ++ show actual_pi
                  ++ ")"
    else
      Left
        ( "ERROR Type Mismatch : CheckStatement -> LetStatement of variable "
            ++ x
            ++ " Expected type "
            ++ (show (gtType t_x))
            ++ " but got "
            ++ show (gtType t_t)
        )
-- Inference:
-- -- CT; C; Γ; ∆ ⊢ t : Bool π ▷ Γ′         CT; C; Γ′; ∆ ⊢ s_i ▷ Γ_i′         Γ′′ = Γ′_1 ⊓ Γ′_2
-- -- -----------------------------------------------------------------------------------------
-- -- CT; C; Γ; ∆ ⊢ if t then s1 else s2 ▷ Γ′′
checkStatement ct gamma pEnv delta (IfStatements t s0 s1) = do
  (t_t, gamma', pEnv') <- checkTerm ct gamma pEnv delta t
  if gtType t_t /= BoolGroundType
    then
      Left
        ( "ERROR Type Mismatch : checkStatement -> IfStatement"
            ++ " Expected type "
            ++ (show BoolGroundType)
            ++ " but got "
            ++ show (gtType t_t)
        )
    else do
      -- TODO(Joan) Modify Delta and pass to successive checkStatements - Joan
      (gamma_1', pEnv_1) <- checkStatement ct gamma' pEnv' delta s0 -- CT; C; Γ′; ∆ ⊢ s_0 ▷ Γ′_1, pEnv_1
      (gamma_2', pEnv_2) <- checkStatement ct gamma' pEnv' delta s1 -- CT; C; Γ′; ∆ ⊢ s_1 ▷ Γ′_2, pEnv_2
      -- intersectionGamma gamma_1' gamma_2' -- Γ′′ = Γ′_1 ⊓ Γ′_2
      gamma_final <- intersectionGamma gamma_1' gamma_2' -- Γ′′ = Γ′_1 ⊓ Γ′_2
      -- TODO(Joan) perform intersection of pEnv_1 and pEnv_2 - Joan
      let pEnv_final = Map.intersectionWith max pEnv_1 pEnv_2
      return (gamma_final, pEnv_final)
-- Inference:
-- -- CT; C; Γ; ∆ ⊢ t : Bool π ▷ Γ′
-- -- CT; C; Γ′; ∆ ⊢ s ▷ Γ′′
-- -- Γ′′′ = Γ′ ⊓ Γ′′
-- -- CT; C; Γ′′′; ∆ ⊢ t : Bool π′ ▷ Γ′′′
-- -- CT; C; Γ′′′; ∆ ⊢ s ▷ Γ′′
-----------------------------------------------------
-- -- CT; C; Γ; ∆ ⊢ do s while t ▷ Γ′′′
checkStatement ct gamma pEnv delta (WhileStatements t s) = do
  (t_t, gamma', pEnv') <- checkTerm ct gamma pEnv delta t -- CT; C;    Γ; ∆ ⊢ t : Bool π ▷ Γ′
  if gtType t_t /= BoolGroundType --  t : Bool
    then
      Left
        ( "ERROR Type Mismatch : checkStatement -> WhileStatements under gamma'"
            ++ " Expected type "
            ++ (show BoolGroundType)
            ++ " but got "
            ++ show (gtType t_t)
        )
    else do
      (gamma'', pEnv'') <- checkStatement ct gamma' pEnv' delta s -- CT; C;   Γ′; ∆ ⊢ s ▷ Γ′′
      gamma''' <- intersectionGamma gamma' gamma'' -- Γ′′′ = Γ′ ⊓ Γ′′
      -- TODO() Perform intersetion of pEnv' and pEnv'' .... Ignore for now - Joan
      let pEnv''' = Map.intersectionWith max pEnv' pEnv''

      (t_t', gamma'''_again, pEnv'''_again) <- checkTerm ct gamma''' pEnv''' delta t -- CT; C; Γ′′′; ∆ ⊢ t : Bool π′ ▷ Γ′′′
      if gtType t_t' /= BoolGroundType --  t : Bool
        then
          Left
            ( "ERROR Type Mismatch : checkStatement -> WhileStatements under gamma'''"
                ++ " Expected type "
                ++ (show BoolGroundType)
                ++ " but got "
                ++ show (gtType t_t')
            )
        else do
          (gamma'''', pEnv'''') <- checkStatement ct gamma'''_again pEnv'''_again delta s
          return (gamma'''_again, pEnv'''_again)

-- Inference
-- -- CT; C; Γ; ∆ ⊢ t : Bool π ▷ Γ′
-- -- CT; C; Γ′; ∆ ⊢ s ▷ Γ′′
-- -- Γ′′′ = Γ′ ⊓ Γ′′
-- -- CT; C; Γ′′′; ∆ ⊢ t : Bool π′ ▷ Γ′′′
-- -- CT; C; Γ′′′; ∆ ⊢ s ▷ Γ′′′
-----------------------------------------
-- -- CT; C; Γ; ∆ ⊢ while t do s ▷ Γ′′′
checkStatement ct gamma pEnv delta (DoStatements s t) = do
  (t_t, gamma', pEnv') <- checkTerm ct gamma pEnv delta t -- CT; C;    Γ; ∆ ⊢ t : Bool π ▷ Γ′
  if gtType t_t /= BoolGroundType --  t : Bool
    then
      Left
        ( "ERROR Type Mismatch : checkStatement -> DoStatements under gamma'"
            ++ " Expected type "
            ++ (show BoolGroundType)
            ++ " but got "
            ++ show (gtType t_t)
        )
    else do
      (gamma'', pEnv'') <- checkStatement ct gamma' pEnv' delta s -- CT; C;   Γ′; ∆ ⊢ s ▷ Γ′′
      gamma''' <- intersectionGamma gamma' gamma'' -- Γ′′′ = Γ′ ⊓ Γ′′
      let pEnv''' = Map.intersectionWith min pEnv' pEnv''

      (t_t', gamma'''_again, pEnv'''_again) <- checkTerm ct gamma''' pEnv''' delta t -- CT; C; Γ′′′; ∆ ⊢ t : Bool π′ ▷ Γ′′′
      if gtType t_t' /= BoolGroundType --  t : Bool
        then
          Left
            ( "ERROR Type Mismatch : checkStatement -> DoStatements under gamma'''"
                ++ " Expected type "
                ++ (show BoolGroundType)
                ++ " but got "
                ++ show (gtType t_t')
            )
        else do
          (gamma'''', pEnv'''') <- checkStatement ct gamma'''_again pEnv'''_again delta s
          return (gamma'''_again, pEnv'''_again)
-- Inference:
-- -- CT; C; Γ; ∆ ⊢ s1 ▷ Γ1        CT; C; Γ; ∆ ⊢ s2 ▷ Γ2
-- -- --------------------------------------------------
-- -- CT; C; Γ; ∆ ⊢ s1; s2 ▷ Γ2
checkStatement ct gamma pEnv delta (BlockStatements statement) =
  checkBlock gamma pEnv statement
  where
    checkBlock currentGamma currentPEnv [] =
      Right (currentGamma, currentPEnv)
    checkBlock currentGamma currentPEnv (s : ss) = do
      (nextGamma, nextPEnv) <- checkStatement ct currentGamma currentPEnv delta s
      checkBlock nextGamma nextPEnv ss
checkStatement ct gamma pEnv delta (ReturnStatements t) = do
  (t_t, gamma', pEnv') <- checkTerm ct gamma pEnv delta t
  return (gamma', pEnv')
-- Inference:
-- -- Γ(x) = Cπ
----------------------------------------
-- -- Γ ⊢ x.grant(p) ▷ Γ[x → C(π ∪ p)]
checkStatement ct gamma pEnv delta (XGrantStatements x p) = do
  let ty = Map.lookup x gamma
  case ty of
    Nothing -> Left ("Error: checkStatement -> XGrantStatements variable " ++ x ++ " not found in gamma")
    Just ty' -> do
      let pi' = (piType ty') {purposes = Set.toList (Set.insert p (Set.fromList (purposes (piType ty'))))}
      return (Map.insert x (ty' {piType = pi'}) gamma, pEnv)
-- Inference:
-- -- Γ(x) = Cπ         WARN : p ∈ π
----------------------------------------
-- -- Γ ⊢ x.revoke(p) ▷ Γ[x → C(π \ p)]
checkStatement ct gamma pEnv delta (XRevokeStatements x p) = do
  let ty = Map.lookup x gamma
  case ty of
    Nothing -> Left ("Error: checkStatement -> XRevokeStatements variable " ++ x ++ " was not found in gamma")
    Just ty' -> do
      let pi' = (piType ty') {purposes = List.delete p (purposes (piType ty'))}
      return (Map.insert x (ty' {piType = pi'}) gamma, pEnv)
-- checkStatement ct gamma pEnv delta (XSetStateStatements x s) = do
--   let ty = Map.lookup x pEnv
--   case ty of
--     Nothing -> do
--         let pEnv' = Map.insert x s pEnv -- if its not found make it ActiveState
--         return (gamma, pEnv')
--     Just ty' -> do
--         let pEnv' = Map.insert x s pEnv -- if found replace with the s given
--         return (gamma, pEnv')
checkStatement ct gamma pEnv delta (XSetStateStatements x s) = do
  let pEnv' = Map.insert x s pEnv -- if found-or-not replace with the s given
  return (gamma, pEnv')

-- Inference:
-- -- CT; C; Γ; ∆ ⊢ t : Tt ▷ Γ′          Γ′(x) = T_x           T_x ⊆ T_t
-----------------------------------------------------------------------
-- -- CT; C; Γ; ∆ ⊢ x ..= t ▷ Γ′
checkStatement ct gamma pEnv delta (XAssignmentStatements x t) = do
  (t_t, gamma', pEnv') <- checkTerm ct gamma pEnv delta t
  case Map.lookup x gamma' of
    Just type_x ->
      if isSubType ct (gtType t_t) (gtType type_x)
        then
          if isSubPurpose (piType t_t) (piType type_x)
            then Right (gamma', pEnv')
            else
              -- Left $ "Purpose Misuse: Cannot assign to variable " ++ x
              let expected_pi = piType type_x
                  actual_pi = piType t_t
                  missing =
                    Set.toList
                      ( Set.fromList (purposes expected_pi)
                          `Set.difference` Set.fromList (purposes actual_pi)
                      )
               in Left
                    ( "ERROR Purpose Misuse: XAssignmentStatements for "
                        ++ x
                        ++ ". Missing: "
                        ++ show missing
                        ++ " (Expected "
                        ++ show expected_pi
                        ++ " but got "
                        ++ show actual_pi
                        ++ ")"
                    )
        else
          Left
            ( "ERROR Type Mismatch : checkStatement -> XAssignmentStatements variable "
                ++ x
                ++ " Expected type "
                ++ (show (gtType type_x))
                ++ " but got "
                ++ show (gtType t_t)
            )
    Nothing -> Left ("ERROR: checkStatement -> XAssignmentsStatements Variable " ++ x ++ " not found in gamma")
-- Inference:
-- -- Γ ⊢ t1 : Cπ ▷ Γ'
-- -- CT(C, f ) = G
-- -- Γ′ ⊢ t2 : Gπ′ ▷ Γ′′
-- -- π ⊑ π'
----------------------------
-- -- Γ ⊢ t1.f := t_2 ▷ Γ′
checkStatement ct gamma pEnv delta (TAssignmentStatements target expr) = do
  (t_t, gamma', pEnv') <- checkTerm ct gamma pEnv delta target -- Γ ⊢ t1 : Cπ ▷ Γ'
  (t_t2, gamma'', pEnv'') <- checkTerm ct gamma' pEnv' delta expr -- Γ′ ⊢ t2 : Gπ′ ▷ Γ′′
  if isSubType ct (gtType t_t2) (gtType t_t)
    then
      if isSubPurpose (piType t_t2) (piType t_t) -- π ⊑ π'
        then Right (gamma'', pEnv'')
        else
          let expected_pi = piType t_t
              actual_pi = piType t_t2
              missing =
                Set.toList
                  ( Set.fromList (purposes expected_pi)
                      `Set.difference` Set.fromList (purposes actual_pi)
                  )
           in Left $
                "ERROR Purpose Misuse: checkStatement -> TAssignmentStatements "
                  ++ "Missing purposes: "
                  ++ show missing
                  ++ " (Expected "
                  ++ show expected_pi
                  ++ " but got "
                  ++ show actual_pi
                  ++ ")"
    else
      Left $
        "ERROR Type Mismatch: checkStatement -> TAssignmentStatements "
          ++ "Expected "
          ++ show (gtType t_t)
          ++ " but got "
          ++ show (gtType t_t2)
-- Inference:
-- -- Γ ⊢ t : C_t π_t ▷ Γ′           Γ′ i ⊢ x_i : G_x,_i  π_x,_i ▷ Γ′′_i
-- -- Γ′_i = Γ′′_i−1
-- -- Γ′_1 = Γ′
-- -- CT(_Ct, m) = ∀ρ_x,_i. (Gx,iπ′x,i => π′′_x,_i) → G_π
-- -- σ = Union_xi pMatch (π′_x,_i, π_x,_i)
-- -- is-fn(σ)
----------------------------------------------------------------
-- -- -- Γ ⊢ t.m(x1 . . . xn) : G(πσ) ▷ Γ′′n[xi 7 → Gx,i(π′′x,iσ)]
-- Syntax: t.m(x)
checkStatement ct gamma pEnv delta (MethodCallStatements t) = do
  (t_t, gamma', pEnv') <- checkTerm ct gamma pEnv delta t
  return (gamma', pEnv')

-- ∅ ⊢ T
-- ∆ ⊢ ť
-- ∆= { ρ }
-----------------------------------
-- CT, C ⊢ M
checkMethodDecl :: ClassTable -> String -> MethodDecl -> CheckMethodDeclResult
checkMethodDecl ct className method = do
  let delta = collectRhos method
  classInfo <- case Map.lookup className ct of
    Just info -> Right info
    Nothing -> Left $ "ERROR: checkMethodDecl -> Class " ++ className ++ " not found in Class Table."

  let localPolicy = classPurposeSetClassInfo classInfo
  let fields = fieldTypeClassInfo classInfo
  let fieldGamma = Map.map (\gt -> Type gt localPolicy) fields

  let argGamma = Map.fromList [(nArgumentDecl arg, tArgumentDecl arg) | arg <- ptxpiMethodDecl method]
  let thisType = Type (ClassGroundType className) AnyPurpose
  let initialGamma = Map.unions [argGamma, fieldGamma, Map.singleton "this" thisType]

  -- TODO()check starting and ending state, and add them as active initially
  let startPurposes = maybe [] psPurposes (sPurposeState method)
  let endPurposes = maybe [] psPurposes (fPurposeState method)
  let allMethodPs = List.nub (startPurposes ++ endPurposes)
  let initialPEnv =
        foldl (\acc p -> Map.insert p ActiveState acc) Map.empty allMethodPs
          `Map.union` ( case sPurposeState method of
                          Nothing -> Map.empty
                          Just (PurposeState ps st) -> Map.fromList [(p, st) | p <- ps]
                      )
  (finalGamma, finalPEnv) <- checkStatement ct initialGamma initialPEnv delta (sMethodDecl method)

  case fPurposeState method of
    Nothing -> Right () -- no state transition expected
    Just (PurposeState ps expected_transition) ->
      mapM_ (checkStateTransition finalPEnv expected_transition) ps

  checkReturn ct initialGamma initialPEnv delta (tMethodDecl method) (sMethodDecl method)
  checkTransitions (ptxpiMethodDecl method) finalGamma
  where
    checkStateTransition env expected p =
      case Map.lookup p env of
        Just actual | actual == expected -> Right ()
        Just actual ->
          Left $
            "ERROR: checkTerm -> MethodCallTerm  -> checkStateTransition -> Final Transition Failed: Purpose "
              ++ p
              ++ " ended in "
              ++ show actual
              ++ " but expected "
              ++ show expected
        Nothing -> do
          if ActiveState == expected
            then Right ()
            else
              Left $
                "ERROR: checkTerm -> MethodCallTerm  -> checkStateTransition -> Final Transition Failed: Purpose "
                  ++ p
                  ++ " ended in "
                  ++ (show ActiveState)
                  ++ " but expected "
                  ++ show expected
    checkTransitions [] _ = Right ()
    checkTransitions (arg : args) fGamma = do
      let x = nArgumentDecl arg
      case Map.lookup x fGamma of
        Just (Type _ pi_actual) ->
          if isSubPurpose pi_actual (piArgumentDecl arg)
            then checkTransitions args fGamma
            else Left "Purpose Transition Violation"
        Nothing -> Left ("ERROR: checkTransitions -> checkMethodDecl -> The variable " ++ x ++ " was not found in the local gamma")

    -- TODO come back to this, peform reacheability in terms of structure
    checkReturn ct g p d expected (BlockStatements stmts) = checkBlock g p stmts
      where
        checkBlock _ _ [] = Right ()
        checkBlock currentG currentP (s : ss) = do
          case s of
            ReturnStatements expr -> do
              (Type gt_actual pi_actual, _, _) <- checkTerm ct currentG currentP d expr
              if isSubType ct gt_actual (gtType expected) && isSubPurpose pi_actual (piType expected)
                then Right ()
                else Left "Type Mismatch in Return"
            _ -> case checkStatement ct currentG currentP d s of
              Right (nextG, nextP) -> checkBlock nextG nextP ss
              Left err -> Left err

    collectRhos m = List.nub $ concatMap (\a -> [getRho (piType (tArgumentDecl a)), getRho (piArgumentDecl a)]) (ptxpiMethodDecl m)

checkConstructorDecl :: ClassTable -> String -> ConstructorDecl -> Either String ()
checkConstructorDecl ct className kDecl = do
  let delta = collectRhos kDecl
  classInfo <- case Map.lookup className ct of
    Just info -> Right info
    Nothing -> Left $ "ERROR: checkConstructorDecl -> Class " ++ className ++ " not found in ClassTable"

  let constructorPi = PurposeSet (extract_purposes_from_ArgumentDecls (ptxpiConstructorDecl kDecl) []) Nothing
  let fieldGamma = Map.map (\gt -> Type gt constructorPi) (fieldTypeClassInfo classInfo)

  let argGamma = Map.fromList [(nArgumentDecl arg, tArgumentDecl arg) | arg <- ptxpiConstructorDecl kDecl]
  let thisType = Type (ClassGroundType className) AnyPurpose
  let initialGamma = Map.unions [argGamma, fieldGamma, Map.singleton "this" thisType]
  let initialPEnv = Map.empty

  (finalGamma, finalPEnv) <- checkStatement ct initialGamma initialPEnv delta (sConstructorDecl kDecl)

  checkTransitions (ptxpiConstructorDecl kDecl) finalGamma
  where
    checkTransitions [] _ = Right ()
    checkTransitions (arg : args) fGamma = do
      let x = nArgumentDecl arg
      case Map.lookup x fGamma of
        Just (Type _ pi_actual) ->
          if isSubPurpose pi_actual (piArgumentDecl arg)
            then checkTransitions args fGamma
            else Left $ "Purpose Transition Violation for variable: " ++ x
        Nothing -> Left ("ERROR: checkTransitions -> checkConstructorDecl -> The variable " ++ x ++ " was not found in the local gamma")

    collectRhos k = List.nub $ concatMap (\a -> [getRho (piType (tArgumentDecl a)), getRho (piArgumentDecl a)]) (ptxpiConstructorDecl k)

checkClass :: ClassTable -> [ClassDecl] -> ClassDecl -> Either String ()
checkClass ct classList cDecl = do
  let className = cClassDecl cDecl
  checkConstructorDecl ct className (kClassDecl cDecl)
  mapM_ (checkMethodDecl ct className) (mClassDecl cDecl)

checkProgram :: Program -> Either String ()
checkProgram (Program classList) = do
  let ct = classTableConstructor (Program classList)
  mapM_ (checkClass ct classList) classList

-- DEBUG remove later
findMethodInAST :: [ClassDecl] -> String -> String -> Maybe MethodDecl
findMethodInAST [] _ _ = Nothing
findMethodInAST (c : cs) className methodName
  | cClassDecl c == className = List.find (\m -> mMethodDecl m == methodName) (mClassDecl c)
  | otherwise = findMethodInAST cs className methodName

-- -- -- -- -- Main   -- -- -- -- --

main :: IO ()
main = do
  let input =
        unlines
          [ "class A extends Object {",
            "  int aIdentity;",
            "  function A (a : int {|p0|TheConstructorRHO|} => {|p1|}) {",
            "    aIdentity := a;",
            "  }",
            "  function aGetter() -> int {|p3|} {",
            "    return a;",
            "  }",
            "}",
            "class B extends A {",
            "  int aIdentity;",
            "  function B(a : int {|p0|TheConstructorRHO|} => {|p1|}) {",
            "    aIdentity := a;",
            "  }",
            "  function aGetter() -> int {|p3|} {",
            "    return a;",
            "  }",
            "}",
            "class B extends A {",
            "  int aIdentity;",
            "  function B(a : int {|p0|TheConstructorRHO|} => {|p1|}) {",
            "    aIdentity := a;",
            "  }",
            "  function aGetter() -> int {|p3|} {",
            "    return a;",
            "  }",
            "}",
            "class C extends Object {",
            "  int aIdentity;",
            "  function C(a : int {|p0|TheConstructorRHO|} => {|p1|}) {",
            "    aIdentity := a;",
            "  }",
            "  function aGetter() -> int {|p3|} {",
            "    return a;",
            "  }",
            "}",
            "class TestNew extends Object {",
            "  int aIdentity;",
            "  function TestNew(a : int {|Internal|} => {|Internal|}) {",
            "    aIdentity := a;",
            "  }",
            "  function identityTestNewBool(tt : bool {|p0|}) -> bool {|p0|} {",
            "   let x : bool {|p0|} := tt in {return x;};",
            "   return this.identityTestNewBool(tt);",
            "   return true;",
            "   aIdentity := 123;",
            "  }",
            "  function identityTestNewBoolInt(tt : bool {|p0|}, i : int {|p1|}) -> bool {|p0|} {",
            "   return tt;",
            "  }",
            "  function identityCompose(tt : bool {|p0|}, i : int {|p1|}) -> bool {|p0|} {",
            "   return this.identityTestNewBool(true);",
            "  }",
            "}"
          ]

  case parse parseProgram "" input of
    Left err -> putStrLn $ "Parser Error: " ++ show err
    Right ast -> do
      let table = classTableConstructor ast
      putStrLn "=========================================================="

      mapM_ printClass (Map.toList table)
      print (isChildClass table "Object" "Object")
      print (isChildClass table "B" "A")

  putStrLn "----------------------------------------------------------"

  let inpTrue = "true"
  case parse parseTerm "" inpTrue of
    Left err -> putStrLn $ "Parse Error: " ++ show err
    Right ast -> do
      let inpTrue' = checkTerm initialClassTable Map.empty Map.empty [] ast
      print inpTrue'

  putStrLn "----------------------------------------------------------"

  let inputLetX = "let x : int {|Internal|} := 5 in {skip;}"
  putStrLn $ "Testing: " ++ inputLetX
  case parse parseLetStatements "" inputLetX of
    Left err -> putStrLn $ "Parse Error: " ++ show err
    Right ast -> do
      print ast
      let inputLetX' = checkStatement initialClassTable Map.empty Map.empty [] ast
      case inputLetX' of
        Right (finalGamma, finalPEnv) -> putStrLn $ "Success! Final Gamma (should be empty): " ++ show finalGamma
        Left err -> putStrLn $ "Type/Security Error: " ++ err

  putStrLn "----------------------------------------------------------"
  -- This should not be rejected?
  let inputLetXYValid = "let x : int {|Internal, Private|} := 5 in {let y : int {|Internal|} := x in {skip;};}"
  putStrLn $ "Testing: " ++ inputLetXYValid
  case parse parseLetStatements "" inputLetXYValid of
    Left err -> putStrLn $ "Parse Error: " ++ show err
    Right ast -> do
      print ast
      let inputLetXYValid' = checkStatement initialClassTable Map.empty Map.empty [] ast
      case inputLetXYValid' of
        Right (finalGamma, finalPEnv) -> putStrLn $ "Success! Final Gamma (should be empty): " ++ show finalGamma
        Left err -> putStrLn $ "Type/Security Error: " ++ err

  putStrLn "----------------------------------------------------------"

  let inputLetXYInValid = "let x : int {|Internal|} := 5 in {let y : int {|Internal, Private|} := x in {skip;};}"
  putStrLn $ "Testing: " ++ inputLetXYInValid
  case parse parseLetStatements "" inputLetXYInValid of
    Left err -> putStrLn $ "Parse Error: " ++ show err
    Right ast -> do
      print ast
      let inputLetXYInValid' = checkStatement initialClassTable Map.empty Map.empty [] ast
      case inputLetXYInValid' of
        Right (finalGamma, finalPEnv) -> putStrLn $ "Success! Final Gamma (should be empty): " ++ show finalGamma
        Left err -> putStrLn $ "Type/Security Error: " ++ err

  putStrLn "----------------------------------------------------------"

  case parse parseProgram "" input of
    Left err -> putStrLn $ "Parser Error: " ++ show err
    Right ast -> do
      let table = classTableConstructor ast
      let inputIfTwoSkip = "if true then { skip; } else { skip; }"
      putStrLn $ "Testing: " ++ inputIfTwoSkip
      case parse parseIfStatements "" inputIfTwoSkip of
        Left err -> putStrLn $ "Parse Error: " ++ show err
        Right ast -> do
          print ast
          let inputIfTwoSkip' = checkStatement table Map.empty Map.empty [] ast
          case inputIfTwoSkip' of
            Right (finalGamma, finalPEnv) -> putStrLn $ "Success! Final Gamma (should be empty): " ++ show finalGamma
            Left err -> putStrLn $ "Type/Security Error: " ++ err

  putStrLn "----------------------------------------------------------"

  case parse parseProgram "" input of
    Left err -> putStrLn $ "Parser Error: " ++ show err
    Right ast -> do
      let table = classTableConstructor ast
      let inputIfTwoLetPass = "if (true) then {let y : int {|p0|} := 123 in {let x : int {| p0 |} := y in {skip;}}} else {let y : int {|p0|} := 123 in {let x : int {| p0 |} := y in {skip;}}}"
      putStrLn $ "Testing: " ++ inputIfTwoLetPass
      case parse parseIfStatements "" inputIfTwoLetPass of
        Left err -> putStrLn $ "Parse Error: " ++ show err
        Right ast -> do
          print ast
          let inputIfTwoSkip' = checkStatement table Map.empty Map.empty [] ast
          case inputIfTwoSkip' of
            Right (finalGamma, finalPEnv) -> putStrLn $ "Success! Final Gamma (should be empty): " ++ show finalGamma
            Left err -> putStrLn $ "Type/Security Error: " ++ err

  putStrLn "----------------------------------------------------------"

  case parse parseProgram "" input of
    Left err -> putStrLn $ "Parser Error: " ++ show err
    Right ast -> do
      let table = classTableConstructor ast
      let nputIfTwoLetRejectThen = "if (true) then {let y : int {|p1|} := 123 in {let x : int {| p0 |} := y in {skip;}}} else {let y : int {|p0|} := 123 in {let x : int {| p0 |} := y in {skip;}}}"
      putStrLn $ "Testing: " ++ nputIfTwoLetRejectThen
      case parse parseIfStatements "" nputIfTwoLetRejectThen of
        Left err -> putStrLn $ "Parse Error: " ++ show err
        Right ast -> do
          print ast
          let inputIfTwoSkip' = checkStatement table Map.empty Map.empty [] ast
          case inputIfTwoSkip' of
            Right (finalGamma, finalPEnv) -> putStrLn $ "Success! Final Gamma (should be empty): " ++ show finalGamma
            Left err -> putStrLn $ "Type/Security Error: " ++ err

  putStrLn "----------------------------------------------------------"

  case parse parseProgram "" input of
    Left err -> putStrLn $ "Parser Error: " ++ show err
    Right ast -> do
      let table = classTableConstructor ast
      let inputIfTwoLetPass = "if (true) then {let y : int {|p0|} := 123 in {let x : int {| p0 |} := y in {skip;}}} else {let y : int {|p1|} := 123 in {let x : int {| p0 |} := y in {skip;}}}"
      putStrLn $ "Testing: " ++ inputIfTwoLetPass
      case parse parseIfStatements "" inputIfTwoLetPass of
        Left err -> putStrLn $ "Parse Error: " ++ show err
        Right ast -> do
          print ast
          let inputIfTwoSkip' = checkStatement table Map.empty Map.empty [] ast
          case inputIfTwoSkip' of
            Right (finalGamma, finalPEnv) -> putStrLn $ "Success! Final Gamma (should be empty): " ++ show finalGamma
            Left err -> putStrLn $ "Type/Security Error: " ++ err

  putStrLn "----------------------------------------------------------"

  case parse parseProgram "" input of
    Left err -> putStrLn $ "Parser Error: " ++ show err
    Right ast -> do
      let table = classTableConstructor ast
      let methodCallIdentity = "let x : bool {|p0|} := true in { my_obj.identityTestNewBool(x); }"
      putStrLn $ "Testing: " ++ methodCallIdentity

      case parse parseStatements "" methodCallIdentity of
        Left err -> putStrLn $ "Parse Error: " ++ show err
        Right astStatements -> do
          print astStatements
          let my_obj_type = Type (ClassGroundType "TestNew") (PurposeSet [] Nothing)
          let my_obj_gamma = Map.fromList [("my_obj", my_obj_type)]

          let methodCallIdentity' = checkStatement table my_obj_gamma Map.empty [] (head astStatements)

          case methodCallIdentity' of
            Right (finalGamma, finalPEnv) -> putStrLn $ "Success!"
            Left err -> putStrLn $ "Type/Security Error: " ++ err

  putStrLn "----------------------------------------------------------"
  case parse parseProgram "" input of
    Left err -> putStrLn $ "Parser Error: " ++ show err
    -- Pattern match 'Program classList' to get the actual list of ClassDecl
    Right (Program classList) -> do
      let table = classTableConstructor (Program classList)

      putStrLn "Testing Declaration: TestNew.identityTestNewBool"
      -- Pass 'classList' (the [ClassDecl]) instead of 'ast' (the Program)
      case findMethodInAST classList "TestNew" "identityTestNewBool" of
        Just mDecl -> do
          let result = checkMethodDecl table "TestNew" mDecl
          case result of
            Right _ -> putStrLn "Result: [PASS] Method body matches signature."
            Left err -> putStrLn ("Result: [FAIL] " ++ err)
        Nothing -> putStrLn "Error: Could not find method declaration in AST."

-- PrettyPrinter for ClassInfo
printClass :: (String, ClassInfo) -> IO ()
printClass (name, info) = do
  putStrLn ("CLASS: " ++ name)
  putStrLn ("  Extends:           " ++ extendsClassInfo info)
  putStrLn ("  Purpose Variables: " ++ show (purposes (classPurposeSetClassInfo info)))

  let fields = Map.keys (fieldTypeClassInfo info)
  putStrLn ("  Fields:            " ++ (if null fields then "None" else show fields))

  putStrLn "  Methods:"
  Map.foldrWithKey
    ( \mName mType acc -> do
        acc
        putStrLn ("    - " ++ mName)
        putStrLn ("      Pretty: " ++ prettyPrinterMethodType mType)
        putStrLn ("      Raw:    " ++ show mType)
    )
    (return ())
    (methodTypeClassInfo info)

  putStrLn "----------------------------------------------------------"
