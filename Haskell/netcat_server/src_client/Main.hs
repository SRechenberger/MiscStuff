{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Conduit
import Data.Conduit.Combinators
import Data.Conduit.Network
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString.Char8 (ByteString, unpack, hGetContents)
import System.IO (IOMode(..), openFile, hClose)
import System.Environment (getArgs)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)
import Control.Concurrent.Async (concurrently)
import Data.String (fromString)

main :: IO ()
main = do
  args <- getArgs
  case args of
    hostport:filename:_ -> runTCPClient
      (clientSettings 4000 (fromString hostport))
      (\server -> do
        let request :: ByteString
            request = fromString $ filename ++ "\r\n"
        void $ concurrently
          (runResourceT $ yield request $$ appSink server)
          (runResourceT $ appSource server $$ stdout))
