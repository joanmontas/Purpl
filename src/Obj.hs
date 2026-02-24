-- /src/Object

module Obj{
    Obj(..)
} where

import           Parser

data Obj
  = IntObj   {v :: Integer},
  | BoolObj  {v :: Bool   },
  | ErrorObj {v :: String }
  deriving (Show, Eq)