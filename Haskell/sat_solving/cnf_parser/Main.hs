{-# LANGUAGE OverloadedStrings #-}
module Main where

import SAT.Formula
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text (Parser)
import Data.Conduit (Source, Conduit, Sink, yield, await, ($$), ($=), (=$))
import Data.Conduit.Text
import Data.Conduit.Binary (sourceFile)
import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad.IO.Class (liftIO)
import System.Environment

import Debug.Trace

import Prelude hiding (lines)

data CNFLine
  = C T.Text
  | P Integer Integer
  | CL Clause
  deriving Show

isC, isP, isCL :: CNFLine -> Bool
isC (C _) = True
isC _ = False

isP (P _ _) = True
isP _ = False

isCL (CL _) = True
isCL _ = False

type CNFFile = [CNFLine]


int :: Parser Integer
int = do
  la <- lookAhead (char '-' <|> digit)
  case la of
    '-' -> char '-' *> ((negate . read) <$> many1 digit)
    _   -> read <$> many1 digit


parseCNFLine :: Monad m
             => Int
             -> Conduit T.Text m (Either ParseError CNFLine)
parseCNFLine lineNum = do
  l <- await
  case l of
    Nothing -> return ()
    Just l -> if T.null l then return () else do
      yield $ case T.head l of
        'c' -> parse (C . T.pack
            <$> (char 'c' *> spaces *> many anyChar)
            <?> ("Line " ++ show lineNum)) "" l
        'p' -> parse
          (P
            <$> (char 'p' *> spaces *> string "cnf" *> spaces *> int)
            <*> (spaces *> int)
            <?> ("Line " ++ show lineNum))
          "" l
        _ -> parse
          (CL
            <$> (manyTill (spaces *> int) (try $ spaces *> char '0'))
            <?> ("Line " ++ show lineNum))
          "" l
      parseCNFLine (lineNum + 1)


parseCNFFile :: (Monad m)
             => Sink
                  (Either ParseError CNFLine)
                  m
                  (Either ParseError CNFFile)
parseCNFFile = do
    l <- await
    case l of
        Nothing  -> return $ Right []
        Just l   -> do
            ls <- parseCNFFile
            return $ (:) <$> l <*> ls

compileFormula :: CNFFile -> Either String Formula
compileFormula file = do
    (vNum, cNum) <- dimensions
    cs <- clauses
    r <- range
    return $ Formula
        { _formClauses   = cNum
        , _formVars = vNum
        , _formVarRange  = r
        , _formFormula   = cs
        }
  where
      dimensions :: Either String (Integer, Integer)
      dimensions = case filter isP file of
          []          -> Left "No dimensions declared."
          (P vs cs):_ -> Right (vs,cs)
      clauses :: Either String [Clause]
      clauses = case filter isCL file of
          []  -> Left "No clauses."
          cls -> Right $ map (\(CL cs) -> cs) cls
      range :: Either String (Integer, Integer)
      range = do
          cls <- map abs . filter (/= 0) . concat <$> clauses
          return (minimum cls, maximum cls)

main :: IO ()
main = do
  args <- getArgs
  case args of
    fname:_ -> do
      r <- runResourceT
          $ sourceFile fname
          $$ decode utf8
          =$ lines
          =$ parseCNFLine 0
          =$ parseCNFFile
      case r of
          Left perr -> putStrLn $ "Parser Error: " ++ fname ++ ":\n" ++ show perr
          Right cnf -> case compileFormula cnf of
              Left cerr  -> putStrLn $ "Error: " ++ fname ++ ": " ++ cerr
              Right form -> print form
    _ -> putStrLn "Usage: ..."
