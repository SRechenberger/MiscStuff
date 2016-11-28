module Solver where

import Data.DIMACS

import Control.Lens

import Control.Monad.State
import Control.Monad.Random

import Data.Set (Set)
import qualified Data.Set as S

type Solver g a = StateT Output (Rand g) a

runSolver :: RandomGen g => g -> Solver g a -> Output
runSolver gen solver = evalRand (execStateT solver (Output [] UNKNOWN S.empty)) gen

solve :: RandomGen g => g -> CNF -> Output
solve gen cnf = runSolver gen
    (solve' (S.fromList [1..(cnf ^. numVariables)])
    (cnf ^. clauses))

solve' :: RandomGen g => Set Int -> [Clause] -> Solver g Bool
solve' vars clauses
  | null clauses = solution .= SATISFIABLE >> return True
  | S.empty `elem` clauses = solution .= UNSATISFIABLE >> return False
  | otherwise = do
      let units = S.filter (\v -> S.singleton v `elem` clauses) vars
      if not $ S.null units
         then do
           forM_ units $ \u -> values %= S.insert u
           solve' (vars S.\\ units) $
            foldl
              (\cs u -> if u < 0 then set (-u) False cs else set u True cs)
              clauses units
         else do
          (v,x) <- lift $ do
            v <- uniform $ S.toList vars
            x <- uniform [True, False]
            return (v,x)

          let lit = if x then v else -v
          sat <- solve' (S.delete v vars) (set v x clauses)
          if sat
            then values %= S.insert lit >> return True
            else do
              sat' <- solve' (S.delete v vars) (set v (not x) clauses)
              if sat'
                then values %= S.insert (-lit) >> return True
                else return False

  where
    set :: Int -> Bool -> [Clause] -> [Clause]
    set var True = filter (var `S.notMember`) . map (S.filter (/= (-var)))
    set var False = set (-var) True

