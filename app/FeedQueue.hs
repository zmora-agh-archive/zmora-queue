{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.ProtocolBuffers
import           Network.AMQP
import           Zmora.AMQP
import           Zmora.Queue

testTask :: Task
testTask =
  Task
  { taskId = putField $ Just 1
  , configuration = putField $ Just "dummy config string"
  , files = putField [sourceFile]
  , tests = putField []
  }
  where
    sourceFile =
      File
      { name = putField $ Just "source.c"
      , content = putField $ Just "void main() {}"
      }

brokerURI :: String
brokerURI = "amqp://guest:guest@localhost:5672"

main :: IO ()
main = do
  putStrLn $ "Sending task " ++ show testTask
  withConnection (fromURI brokerURI) $ \connection ->
    withTaskPublisher connection $ \publisher -> do
      result <- publish publisher testTask
      putStrLn $ "(acks, nacks, [pending]): " ++ show result
