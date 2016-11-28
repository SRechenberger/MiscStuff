{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.DIMACS

import Data.Conduit
import qualified Data.Conduit.Combinators as C

import Data.Text (Text)
import qualified Data.Text as T

import Control.Lens

import System.Console.GetOpt
import System.Environment
import System.Random
import System.Exit

import Control.Monad.Trans.Resource
import Data.Maybe
import Text.Read
import Data.Monoid

import Solver


type InStream = Source (ResourceT IO) Text
type OutStream = Sink Text (ResourceT IO) ()

data Flags = Flags
  { _source :: InStream
  , _sourcePath :: FilePath
  , _target :: OutStream
  , _targetPath :: FilePath
  , _gen    :: IO StdGen
  }

makeLenses ''Flags

defFlags :: Flags
defFlags = Flags
  { _source = C.stdin
  , _sourcePath = "stdin"
  , _target = C.stdout
  , _targetPath = "stdout"
  , _gen    = getStdGen
  }

options :: [OptDescr (Flags -> Flags)]
options =
  [ Option ['s'] ["source"]
    (ReqArg (\fp -> (source .~ C.sourceFile fp) . (sourcePath .~ fp)) "INPUT_FILE")
      "input formula in dimacs cnf format; default: read from stdin"
  , Option ['t'] ["target"]
    (ReqArg (\fp -> (target .~ C.sinkFile fp) . (targetPath .~ fp)) "OUTPUT_FILE") $
      "output file formatted according to "
      ++ "http:\\baldur.iti.kit.edu/sat-competition-2016/index.php?cat=certificates;"
      ++ "default: write to stdout"
  , Option ['r'] ["randseed"]
    (ReqArg (\seed -> gen .~ (fromMaybe getStdGen $ do
        i <- readMaybe seed
        return (return $ mkStdGen i)))
      "SEED")
    "random seed"
  ]

interpretOpts :: [String] -> IO Flags
interpretOpts args =
  case getOpt Permute options args of
    (fs,_,[]) -> return $ foldr id defFlags fs
    (_,_,err) -> ioError
      $ userError
      $ concat err ++ usageInfo "Usage: dpll [OPTIONS...]" options


main :: IO ()
main = do
  flags <- getArgs >>= interpretOpts
  gen <- flags ^. gen
  s <- runResourceT $ do
    cnf <- flags ^. source $$ cnfSink
    let o = solve gen cnf
              & comments
                %~ (++ [ "from"
                       , " " <> (T.pack $ flags ^. sourcePath)
                       , "to"
                       , " " <> (T.pack $ flags ^. targetPath)
                       ])
    outputSource o $$ flags ^. target
    return $ o ^. solution
  case s of
    SATISFIABLE   -> exitSuccess
    UNSATISFIABLE -> exitWith $ ExitFailure 10
    UNKNOWN       -> exitWith $ ExitFailure 20
