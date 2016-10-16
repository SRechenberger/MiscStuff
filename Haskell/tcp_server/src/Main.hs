{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Conduit
import Data.Conduit.Network
import Data.Conduit.Process

main :: IO ()
main = runTCPServer (serverSettings 9090 "*") $ \appData -> do
    putStrLn "Connection Accepted..."
    (exit, _, _) <- sourceCmdWithStreams "cat" (appSource appData) (appSink appData) (appSink appData)
    putStrLn "Connection Closed..."
    return ()

