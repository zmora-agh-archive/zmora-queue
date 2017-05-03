{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.AMQP (fromURI)
import           Zmora.Queue
import           Zmora.AMQP

testTask :: Task
testTask = Task 1 "dummy config string" [File "source.c" "void main() {}"] []

brokerURI :: String
brokerURI = "amqp://guest:guest@localhost:5672"

main :: IO ()
main = do
  putStrLn $ "Sending task " ++ show testTask
  withConnection (fromURI brokerURI) $ \connection ->
    withTaskPublisher connection $ \publisher -> do
      result <- publish publisher testTask
      putStrLn $ "(acks, nacks, [pending]): " ++ show result
