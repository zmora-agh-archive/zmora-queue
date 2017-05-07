{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE OverloadedStrings  #-}

module Zmora.Queue where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy as BS
import           Data.Int
import           Data.MessagePack
import qualified Data.Text            as T
import           GHC.Generics

--
-- Data model
---
data Task = Task
  { taskId        :: Int64
  , configuration :: String
  , files         :: [File]
  , tests         :: [Test]
  } deriving (Show)

data File = File
  { name    :: T.Text
  , content :: B.ByteString
  } deriving (Show)

data Test = Test
  { testId    :: Int64
  , input     :: T.Text
  , output    :: T.Text
  , timeLimit :: Int
  , ramLimit  :: Int
  } deriving (Show)

data TaskResult = TaskResult
  { resultId       :: Int64
  , compilationLog :: String
  , testResults    :: [TestResult]
  } deriving (Show)

data TestResult = TestResult
  { sourceTestId  :: Int64
  , status        :: Status
  , executionTime :: Int
  , ramUsage      :: Int
  } deriving (Show)

data Status
  = OK
  | RTE
  | MEM
  | TLE
  | ANS
  | CME
  deriving (Eq, Show)

--
-- Serialization
--
deriving instance Generic File
deriving instance Generic Test
deriving instance Generic Task
deriving instance Generic TestResult
deriving instance Generic TaskResult
deriving instance Generic Status
instance MessagePack TaskResult
instance MessagePack File
instance MessagePack Test
instance MessagePack Task
instance MessagePack TestResult
instance MessagePack Status

defaultSerializer :: MessagePack m => m -> BS.ByteString
defaultSerializer = pack

defaultDeserializer :: MessagePack a => BS.ByteString -> IO a
defaultDeserializer = unpack
