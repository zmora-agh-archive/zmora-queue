module Main where

import           Data.MessagePack
import           Network.AMQP
import           Zmora.AMQP       hiding (taskResultSubscriber)
import           Zmora.Queue

handleMessage
  :: (MessagePack a, Show a)
  => Bool -> (a, Envelope) -> IO ()
handleMessage requeue (msg, envelope) = do
  putStrLn $ "Received message: " ++ show msg
  if requeue
    then rejectEnv envelope True
    else ackEnv envelope

passiveSubscriber
  :: MessagePack a
  => QueueOpts -> Connection -> IO (Subscriber IO a)
passiveSubscriber queueOpts connection =
  openChannel connection >>= newSubscriber spec
  where
    spec = SubscriberSpec (queueOpts {queuePassive = True}) defaultDeserializer

taskSubscriber :: Connection -> IO (Subscriber IO Task)
taskSubscriber = passiveSubscriber taskQueueOpts

taskResultSubscriber :: Connection -> IO (Subscriber IO TaskResult)
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
