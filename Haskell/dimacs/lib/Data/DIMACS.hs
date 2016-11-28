{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
module Data.DIMACS where

import qualified Data.Attoparsec.Text as AT (takeWhile)
import Data.Attoparsec.Text hiding (takeWhile)

import Data.Set (Set)
import qualified Data.Set as S

import Data.Text (Text)
import qualified Data.Text as T

import Control.Lens

import Control.Monad.State
import Control.Monad.Catch (MonadThrow)
import Control.Applicative

import Data.Conduit
import Data.Conduit.Attoparsec
import qualified Data.Conduit.Combinators as C

import Control.Monad.Trans.Resource (runResourceT)

import Data.Monoid

data CNF = CNF
  { _numVariables :: !Int
  , _numClauses   :: !Int
  , _clauses      :: ![Clause]
  } deriving Show

type Clause = Set Int

makeLenses ''CNF

cnf' :: StateT CNF Parser ()
cnf' = do
  lift $ skipSpace
  la <- lift $ peekChar
  case la of
    Just 'c' -> do
      lift $ skipWhile (/= '\n') >> char '\n'
      cnf'
    Just 'p' -> do
      (vars, cls) <- lift $ do
        char 'p' >> skipSpace
        string "cnf" >> skipSpace
        (,) <$> (decimal <* skipSpace) <*> (decimal <* skipSpace)
      numVariables .= vars
      numClauses .= cls
      cnf'
    Just _ -> do
      ls <- lift $ (signed decimal <* skipSpace) `manyTill` char '0'
      clauses %= (++ [S.fromList ls])
      cnf'
    Nothing -> return ()

cnf :: Parser CNF
cnf = execStateT cnf' (CNF 0 0 [])

cnfSink :: MonadThrow m => Consumer Text m CNF
cnfSink = sinkParser cnf

cnfFromStdin :: IO CNF
cnfFromStdin = C.stdin $$ cnfSink

cnfFromFile :: FilePath -> IO CNF
cnfFromFile fp = runResourceT $ C.sourceFile fp $$ cnfSink

data Solution = SATISFIABLE | UNSATISFIABLE | UNKNOWN
  deriving (Show, Eq, Enum, Read)

data Output = Output
  { _comments :: [Text]
  , _solution :: Solution
  , _values   :: Set Int
  } deriving Show

makeLenses ''Output

output' :: StateT Output Parser ()
output' = do
  la <- lift $ skipSpace >> peekChar
  case la of
    Just 'c' -> do
      lift $ char 'c' >> skipSpace
      c <- lift $ AT.takeWhile (/= '\n') <* char '\n'
      output'
      comments %= (++ [c])
    Just 's' -> do
      s <- lift $ do
        char 's' >> skipSpace
        (string "SATISFIABLE"
          <|> try (string "UNKNOWN")
          <|> string "UNSATISFIABLE")
      solution .= read (T.unpack s)
      output'
    Just 'v' -> do
      lift $ char 'v' >> skipSpace
      vs <- lift $ (skipSpace *> signed decimal) `manyTill` (char '\n')
      values %= (`S.union` S.fromList vs)
      output'
    Just '\n' -> lift (char '\n') >> output'
    Nothing -> return ()

output :: Parser Output
output = execStateT output' (Output [] UNKNOWN S.empty)

outputSink :: MonadThrow m => Consumer Text m Output
outputSink = sinkParser output

outputSource :: Monad m => Output -> Producer m Text
outputSource output = (do
  forM_ (output ^. comments) $ \c -> yield ("c " <> c)
  yield $ "s " <> T.pack (show (output ^. solution))
  when (output ^. solution == SATISFIABLE) $
    yield $ "v " <> (T.unwords $ map (T.pack . show) $ S.toList $ output ^. values))
  =$= C.unlines
