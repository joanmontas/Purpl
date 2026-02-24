-- src/Scope.hs

module Scope(
    Scope(..)
 ) where

-- import           Object
import Data.IORef
import qualified Data.Map as Map

data Object
  = IntObj   {vInt :: Integer}
  | BoolObj  {vBool :: Bool   }
  | ErrorObj {vError :: String }
  deriving (Show, Eq)

data Scope
  = LocalScope {local :: (Map.Map String (IORef Object)), prev :: Scope}
  | EmptyScope

-- data Scope
--   = LocalScope {local :: (Map.Map String Object), prev :: Scope}
--   | EmptyScope


-- For now, we will pass by value

-- Static Scoping: 

--                 Str to Object
--                 Int to Object <- "Reference, thats not what we want"


-- data Scope
--   = LocalScope {local :: (Map.Map String Object), prev :: Scope}
--   | EmptyScope

-- -- make a local scope, i.e non-empty prev
-- makeLocalScope :: Scope -> Scope
-- makeLocalScope s = LocalScope (Map.empty) s

-- -- insert a Object to the given scope
-- insertObjectToScope :: Scope -> String -> Object -> Scope
-- insertObjectToScope s k o = LocalScope (Map.insert k o (local s)) (prev s)

-- -- insert a object to the global Scope
-- insertObjectToGlobalScope :: Scope -> String -> Object -> Scope
-- insertObjectToGlobalScope s k o = case (prev s) of
--   EmptyScope -> insertObjectToScope s k o
--   otherwise -> LocalScope (local s) (insertObjectToGlobalScope (prev s) k o)

-- -- find a Object via lexically scoping, how I like to call it "bubble up"
-- findObject :: Scope -> String -> Object
-- findObject EmptyScope k = A (EvalErrorAtom "EvalErrorAtom ERROR: Variable not found in any scope")
-- findObject s k = case Map.lookup k (local s) of
--   Just t -> t
--   Nothing -> findObject (prev s) k

-- getPrev :: Scope -> Scope
-- getPrev s = prev s

-- -- make a local scope, i.e non-empty prev
-- makeLocalScope :: Scope -> Scope
-- makeLocalScope s = LocalScope (Map.empty) s

-- -- insert a Object to the given scope
-- insertObjectToScope :: Scope -> String -> Objectression -> Scope
-- insertObjectToScope s k o = LocalScope (Map.insert k o (local s)) (prev s)

-- -- insert a Object to the global Scope
-- insertObjectToGlobalScope :: Scope -> String -> Objectression -> Scope
-- insertObjectToGlobalScope s k o = case (prev s) of
--   EmptyScope -> insertObjectToScope s k o
--   otherwise -> LocalScope (local s) (insertObjectToGlobalScope (prev s) k o)

-- -- find a Object via lexically scoping, how I like to call it "bubble up"
-- findObject :: Scope -> String -> Object
-- findObject EmptyScope k = (ErrorObj "findObject ERROR: Variable not found in any scope")
-- findObject s k = case Map.lookup k (local s) of
--   Just t -> t
--   Nothing -> findObject (prev s) k

-- getPrev :: Scope -> Scope
-- getPrev s = prev s