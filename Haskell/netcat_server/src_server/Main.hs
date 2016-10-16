{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Conduit
import Data.Conduit.Network
import Control.Monad.Trans.Resource
import Data.ByteString.Char8 (ByteString, unpack, hGetContents)
import System.IO (IOMode(..), openFile, hClose)
import Control.Monad.IO.Class (liftIO)

file :: Conduit ByteString (ResourceT IO) ByteString
file = do
  mPath <- await
  case mPath of
    Nothing -> yield "File not found"
    Just path -> bracketP
      (do
        let path' = takeWhile (/= '\r') $ unpack path
        openFile path' ReadMode)
      hClose
      (\handle -> liftIO (hGetContents handle) >>= yield)

main :: IO ()
main = runTCPServer (serverSettings 4000 "*") $ \ad ->
  runResourceT $ appSource ad $$ file =$ appSink ad

