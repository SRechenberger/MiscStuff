module Solver where

import Data.DIMACS

import Control.Lens

import Control.Monad.State
import Control.Monad.Random

import Data.Set (Set)
import qualified Data.Set as S
import Data.List
import Data.Function

import Data.Text (pack)

type Solver g a = StateT (Int, Output) (Rand g) a

runSolver :: RandomGen g => g -> Solver g a -> Output
runSolver gen solver = snd $
  evalRand (execStateT solver (0,(Output [] UNKNOWN S.empty))) gen

solve :: RandomGen g => g -> CNF -> Output
solve gen cnf = runSolver gen
    (do solve' (S.fromList [1..(cnf ^. numVariables)]) (cnf ^. clauses)
        bts <- use _1
        comment $ "Backtracks " ++ show bts)

comment :: RandomGen g => String -> Solver g ()
comment c = _2 . comments %= (++ [pack c])

solve' :: RandomGen g => Set Int -> [Clause] -> Solver g Bool
solve' vars clauses
  | null clauses = _2 . solution .= SATISFIABLE >> return True
  | S.empty `elem` clauses = _2 . solution .= UNSATISFIABLE >> return False
  | otherwise = do
      (v,x) <- lift $ do
        let clauses' = sortBy (compare `on` S.size) clauses
        v <- uniform $ S.toList (head clauses')
        x <- uniform [True, False]
        return (v,x)
      let lit = if x then v else -v
      sat <- solve' (S.delete v vars) (set v x clauses)
      if sat
        then _2 . values %= S.insert lit >> return True
        else do
          _1 += 1
          sat' <- solve' (S.delete v vars) (set v (not x) clauses)
          if sat'
            then _2 . values %= S.insert (-lit) >> return True
            else return False

  where
    set :: Int -> Bool -> [Clause] -> [Clause]
    set var True = filter (var `S.notMember`) . map (S.filter (/= (-var)))
    set var False = set (-var) True

