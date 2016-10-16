{-# LANGUAGE TemplateHaskell #-}
module SAT.Formula where

import qualified Data.Map as M
import Data.Maybe (isJust)

import Control.Lens

type Clause = [Integer]
data Formula = Formula
    { _formClauses :: Integer
    , _formVars :: Integer
    , _formVarRange :: (Integer, Integer)
    , _formFormula :: [Clause]
    }
  deriving (Show, Read)

makeLenses ''Formula

data Assignment = Asgn
    { _asgnVars :: Integer
    , _asgnVarRange :: (Integer, Integer)
    , _asgnMapping :: M.Map Integer Bool
    }
  deriving (Show, Read)

makeLenses ''Assignment

makeEmptyAssignment :: Formula -> Assignment
makeEmptyAssignment frm = Asgn
    { _asgnVars = frm ^. formVars
    , _asgnVarRange = frm ^. formVarRange
    , _asgnMapping = M.empty
    }

applyAssignment :: Assignment -> Formula -> Formula
applyAssignment a = over formFormula (filter (any  (`M.member` trueVars)))
                  . over (formFormula . mapped) (filter (`M.notMember` falseVars))
  where
      trueVars :: M.Map Integer Bool
      trueVars = M.filter id (a ^. asgnMapping)

      falseVars :: M.Map Integer Bool
      falseVars = M.filter not (a ^. asgnMapping)

-- Custom lenses
valOf i = asgnMapping . at i

assigned i = isJust . (^. valOf i)
