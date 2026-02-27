module TypeCheckerUtils where

import Data.Set qualified as Set
import qualified Data.Map as Map
import Ast
import TypeCheckerTypes

-- Description: Go up the tree until the base class is found or not.
isChildClass :: ClassTable -> String -> String -> Bool
isChildClass ct child base =
  if (child == base)
    then True
    else case (Map.lookup child ct) of
      Nothing -> False
      Just info -> isChildClass ct (extendsClassInfo info) base

-- Description: T0 <: T1
isSubType :: ClassTable -> GroundType -> GroundType -> Bool
isSubType ct BoolGroundType BoolGroundType = True
isSubType ct IntGroundType IntGroundType = True
isSubType ct StringGroundType StringGroundType = True
isSubType ct UnitGroundType UnitGroundType = True -- NOTE() Added
isSubType ct (ClassGroundType n0) (ClassGroundType n1) =
  isChildClass ct n0 n1
isSubType ct _ _ = False

-- Description: P0 ⊆ P1
-- -- The rho is accounted for. TODO(Joan) Come back to this - Joan
isSubPurpose :: PurposeSet -> PurposeSet -> Bool
isSubPurpose AnyPurpose _ = True
isSubPurpose _ AnyPurpose = True
isSubPurpose (PurposeSet pSource r0) (PurposeSet pTarget r1) =
  (all (`elem` pSource) pTarget)
    && ( case (r0, r1) of
           (Nothing, _) -> True -- P0 contains less rho purpose
           (Just r0', Just r1') -> r0' == r1' -- They contain the same rho
           (_, _) -> False -- P0 contains more rho purpose
       )

-- | Description: Γ′′ = Γ′_1 ⊓ Γ′_2
-- | Merges two Gammas using ⊓-Type.
intersectionGamma :: Gamma -> Gamma -> Either String Gamma
intersectionGamma gamma_1' gamma_2' = do
  let gammaIntersect = Map.toList (Map.intersectionWith (,) gamma_1' gamma_2')
  let conflicts = findTypeConflicts gammaIntersect

  if not (null conflicts)
    then Left ("Error: intersectionGamma found Type Conflicts in: " ++ show conflicts)
    else
      Right (Map.fromList [(k, meetType v0 v1) | (k, (v0, v1)) <- gammaIntersect])
  where
    findTypeConflicts [] = []
    findTypeConflicts ((k, (v0, v1)) : ks)
      | gtType v0 /= gtType v1 = k : findTypeConflicts ks
      | otherwise = findTypeConflicts ks

    meetType (Type gt1 pi1) (Type gt2 pi2) =
      -- ⊓-Type
      Type
        { gtType = gt1, -- TODO(Joan) Super Type - Joan
          piType =
            PurposeSet
              { purposes = Set.toList (Set.fromList ((purposes pi1) ++ (purposes pi2))),
                rho = (rho pi1) -- TODO(Joan) Something to think about - Joan
              }
        }

-- -- -- -- -- Helpers Made Global -- -- -- -- --

extract_rho_from_PurposeSet (PurposeSet _ (Just rho)) = rho
extract_rho_from_PurposeSet _ = []

extract_rhos_from_ArgumentDecls [] rhos = rhos
extract_rhos_from_ArgumentDecls (arg : args) rhos =
  let rhoT = extract_rho_from_PurposeSet (piType (tArgumentDecl arg))
      rhoA = extract_rho_from_PurposeSet (piArgumentDecl arg)
      currentFound = []
      rhoT' = if not (null rhoT) then rhoT : currentFound else currentFound
      rhoA' = if not (null rhoA) then rhoA : rhoT' else rhoT'

      newRhos = rhos ++ rhoA'
   in extract_rhos_from_ArgumentDecls args newRhos

extract_purposes_from_ArgumentDecls [] purp = purp
extract_purposes_from_ArgumentDecls (arg : args) purp =
  let t = purposes (piType (tArgumentDecl arg))
      pFromArg = purposes (piArgumentDecl arg)
      pi = purp ++ t ++ pFromArg -- side effect
   in extract_purposes_from_ArgumentDecls args pi

getRho :: PurposeSet -> Maybe String
getRho AnyPurpose = Nothing
getRho (PurposeSet _ r) = r


-- -- Description: Matches a Rho to a PurposeSet. Used during Method-Invocation
-- -- -- pmatch({p1 · · · pk     }, {pm · · ·              pn     }) = ∅
-- -- -- pmatch({p1 · · · pk|ρ   }, {p1 · · · pk, pm · · · pn     }) = {ρ  → {pm · · · pn     }}
-- -- -- pmatch({p1 · · · pk|  ρ1}, {p1 · · · pk, pm · · · pn | ρ2}) = {ρ1 → {pm · · · pn | ρ2}}
pMatch sigma p1 p2 =
    case (rho p1, rho p2) of
    (Nothing, Nothing) -> Right sigma
    (Just r1, Nothing) ->
        let diff = Set.toList (Set.difference (Set.fromList (purposes p2)) (Set.fromList (purposes p1)))
        in Right (Map.insertWith (++) r1 [PurposeSet diff Nothing] sigma)
    (Just r1, Just r2) ->
        let diff = Set.toList (Set.difference (Set.fromList (purposes p2)) (Set.fromList (purposes p1)))
        in Right (Map.insertWith (++) r1 [PurposeSet diff (Just r2)] sigma)
    (_, _) -> Right sigma


-- Description: Implements the is-fn(σ) check. Determine if arguments contains
-- rhos that points to two or more PurposeSet.
-- TODO() Come back to this
is_fn sigma = any hasConflict (Map.elems sigma) -- is-fn(σ)
hasConflict [] = False
hasConflict (x : xs) = not (all (== x) xs)