module Main where

import           Data.ProtocolBuffers (Decode)
import           Network.AMQP
import           Zmora.AMQP           hiding (taskResultSubscriber)
import           Zmora.Queue

handleMessage
  :: (Show a)
  => Bool -> (Either String a, Envelope) -> IO ()
handleMessage requeue (msg, envelope) = do
  either error (\x -> putStrLn $ "Received message: " ++ show x) msg
  if requeue
    then rejectEnv envelope True
    else ackEnv envelope

passiveSubscriber
  :: Decode a
  => QueueOpts -> Connection -> IO (Subscriber (Either String) a)
passiveSubscriber queueOpts connection =
  openChannel connection >>= newSubscriber spec
  where
    spec = SubscriberSpec (queueOpts {queuePassive = True}) deserialize

taskSubscriber :: Connection -> IO (Subscriber (Either String) Task)
taskSubscriber = passiveSubscriber taskQueueOpts

taskResultSubscriber :: Connection -> IO (Subscriber (Either String) TaskResult)
taskResultSubscriber = passiveSubscriber taskResultQueueOpts

brokerURI :: String
brokerURI = "amqp://guest:guest@localhost:5672"

main :: IO ()
main = do
  putStrLn $ "Using options: " ++ brokerURI
  putStrLn "Press RETURN to EXIT GRACEFULLY..."
  withConnection (fromURI brokerURI) $ \connection -> do
    _ <- taskSubscriber connection >>= flip subscribe (handleMessage True)
    _ <- taskResultSubscriber connection >>= flip subscribe (handleMessage True)
    _ <- getLine
    return ()
