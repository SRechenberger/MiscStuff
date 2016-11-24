module Main where

import Control.Monad.Writer
import Data.List (nub)

import qualified Data.Conduit.Text as T
import Data.Conduit
import qualified Data.Conduit.Combinators as C

import Control.Monad.Trans.Resource (runResourceT)

type Literal = Int

type Formula = [[Literal]]

readFormula :: String -> Formula
readFormula = map readClauses . lines
  where
    readClauses = map read . words

setLiteral :: Int -> Bool -> Formula -> Formula
setLiteral i False = filter ((-i) `notElem`) . map (filter (/= i))
setLiteral i True  = filter (i `notElem`) . map (filter (/= (-i)))

type Assignment = [(Int, Bool)]

vars' :: Formula -> [Int]
vars' = nub . map abs . concat

vars :: Formula -> [Int]
vars f = [minimum vs .. maximum vs]
  where
    vs = vars' f

solve :: Formula -> [Assignment]
solve f = map reverse . execWriter . solve' (vars f) [] $ f
  where
    solve' :: [Int] -> Assignment -> Formula -> Writer [Assignment] ()
    solve' [] a [] = tell [a]
    solve' [] a [[]] = return ()
    solve' (v:vs) a f
      | [] `elem` f = return ()
      | otherwise   = do
          solve' vs ((v,False):a) (setLiteral v False f)
          solve' vs ((v,True):a) (setLiteral v True f)

main :: IO ()
main = do
  input <- runResourceT $ C.stdin $$ C.fold
  forM_ (solve (readFormula input)) $ \as -> do
    forM_ as $ \(v,b) -> putStrLn $ show v ++ " " ++ show (fromEnum b)
    putStrLn "#"
