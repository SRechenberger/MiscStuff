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

for = flip map

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
  | not (null units) = do
      let units' = for units $ \u ->
                    if u < 0 
                    then (-u, False) 
                    else (u, True)
          clauses' = foldr (uncurry set) clauses units'
      forM_ units $ \l -> _2 . values %= S.insert l
      solve' (vars S.\\ S.fromList (map abs units)) clauses'
  | not (null pures) = do
      vs <- forM pures $ \p -> do
        _2 . values %= S.insert p
      
        let v = if p < 0 then False else True
        return (abs p, v)

      let vars' = vars S.\\ S.fromList (map abs pures)
      let clauses' = foldr (uncurry set) clauses vs
      solve' vars' clauses'
  | otherwise = do
      (v,x) <- branch'
      let lit = if x then v else -v
      s <- use $ _2 . values
      sat <- solve' (S.delete v vars) (set v x clauses)
      if sat
      then _2 . values %= S.insert lit >> return True
      else do
        _2 . values .= s
        _1 += 1
        sat' <- solve' (S.delete v vars) (set v (not x) clauses)
        if sat'
        then _2 . values %= S.insert (-lit) >> return True
        else do
          _2 . values .= s
          return False


  where
    set :: Int -> Bool -> [Clause] -> [Clause]
    set var True = filter (var `S.notMember`) . map (S.filter (/= (-var)))
    set var False = set (-var) True

    units :: [Int]
    units = concat . map S.toList . filter ((==1) . S.size) $ clauses

    vars' :: Set Int
    vars' = S.unions clauses

    pures :: [Int]
    pures =
      [i | i <- S.toList vars', S.member i vars' && S.notMember (-i) vars']
    
    branch :: RandomGen g => Solver g (Int, Bool)
    branch = lift $ do
      let clauses' = sortBy (compare `on` S.size) clauses
      v <- uniform $ S.toList (head clauses')
      return (v,if v < 0 then False else True)
    
    branch' :: RandomGen g => Solver g (Int, Bool)
    branch' = lift $ do
      let clause = head $ reverse $ sortBy (compare `on` S.size) clauses
      v1 <- uniform $ S.toList clause
      v2 <- uniform $ S.toList clause
      v3 <- uniform $ S.toList clause
      let v = maximumBy (compare `on` cnt) [v1,v2,v3]
      return (abs v, if v < 0 then False else True)
      

    cnt i = sum . map fromEnum . map (\s -> S.member i s) $ clauses
