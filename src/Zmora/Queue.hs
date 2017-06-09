{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Zmora.Queue where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Int             (Int64)
import           Data.ProtocolBuffers
import           Data.Serialize       (runGetLazy, runPutLazy)
import qualified Data.Text            as T
import           GHC.Generics         (Generic)

--
-- Data model
--
data File = File
  { name    :: Optional 1 (Value T.Text)
  , content :: Optional 2 (Value BS.ByteString)
  } deriving (Generic, Show)

data Test = Test
  { testId    :: Optional 1 (Value Int64)
  , input     :: Optional 2 (Value T.Text)
  , output    :: Optional 3 (Value T.Text)
  , timeLimit :: Optional 4 (Value Int64)
  , ramLimit  :: Optional 5 (Value Int64)
  } deriving (Generic, Show)

data Task = Task
  { taskId        :: Optional 1 (Value Int64)
  , configuration :: Optional 2 (Value T.Text)
  , files         :: Repeated 3 (Message File)
  , tests         :: Repeated 4 (Message Test)
  } deriving (Generic, Show)

data TaskResult = TaskResult
  { resultId       :: Optional 1 (Value Int64)
  , compilationLog :: Optional 2 (Value T.Text)
  , testResults    :: Repeated 3 (Message TestResult)
  } deriving (Generic, Show)

data TestResult = TestResult
  { sourceTestId  :: Optional 1 (Value Int64)
  , status        :: Optional 2 (Enumeration Status)
  , executionTime :: Optional 3 (Value Int64)
  , ramUsage      :: Optional 4 (Value Int64)
  } deriving (Generic, Show)

data Status
  = OK
  | RTE
  | MEM
  | TLE
  | ANS
  | CME
  deriving (Enum, Show)

--
-- Serialization
--
instance Encode File

instance Encode Test

instance Encode Task

instance Encode TaskResult

instance Encode TestResult

serialize
  :: Encode a
  => a -> BL.ByteString
serialize = runPutLazy . encodeMessage

--
-- Deserialization
--
instance Decode File

instance Decode Test

instance Decode Task

instance Decode TaskResult

instance Decode TestResult

deserialize
  :: Decode a
  => BL.ByteString -> Either String a
deserialize = runGetLazy decodeMessage
