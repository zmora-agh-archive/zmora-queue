{-# LANGUAGE OverloadedStrings #-}

module Zmora.AMQP where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text            as T
import           Network.AMQP
import           Zmora.Queue

connect :: ConnectionOpts -> IO Channel
connect connectionOpts = openConnection'' connectionOpts >>= openChannel

withConnection :: ConnectionOpts -> (Connection -> IO a) -> IO a
withConnection opts f = do
  connection <- openConnection'' opts
  res <- f connection
  closeConnection connection
  return res

data PublisherSpec m = PublisherSpec
  { pubExchangeOpts :: Maybe ExchangeOpts
  , pubKey          :: T.Text
  , pubSerializer   :: m -> BS.ByteString
  , pubAwaitNanos   :: Maybe Int
  }

data Publisher m =
  Publisher { pubSpec :: PublisherSpec m
            , pubChan :: Channel }

data SubscriberSpec m = SubscriberSpec
  { subOpts         :: QueueOpts
  , subDeserializer :: BS.ByteString -> IO m
  }

data Subscriber m =
  Subscriber { subSpec :: SubscriberSpec m
             , subChan :: Channel }

newPublisher :: PublisherSpec m -> Channel -> IO (Publisher m)
newPublisher spec channel = do
  mapM_ (declareExchange channel) (pubExchangeOpts spec)
  return $ Publisher spec channel

connectPublisher :: ConnectionOpts -> PublisherSpec m -> IO (Publisher m)
connectPublisher opts spec = connect opts >>= newPublisher spec

newSubscriber :: SubscriberSpec m -> Channel -> IO (Subscriber m)
newSubscriber spec channel = do
  _ <- declareQueue channel (subOpts spec)
  return $ Subscriber spec channel

connectSubscriber :: ConnectionOpts -> SubscriberSpec m -> IO (Subscriber m)
connectSubscriber opts spec = connect opts >>= newSubscriber spec

publish :: Publisher m -> m -> IO (Maybe ConfirmationResult)
publish (Publisher (PublisherSpec opts key serializer awaitNanos) channel) msg = do
  _ <- publishMsg
       channel
       (maybe "" exchangeName opts)
       key
       newMsg {msgBody = serializer msg}
  mapM (waitForConfirmsUntil channel) awaitNanos

withPublisher :: Connection -> PublisherSpec m -> (Publisher m -> IO a) -> IO a
withPublisher connection spec f = do
  channel <- openChannel connection
  publisher <- newPublisher spec channel
  res <- f publisher
  closeChannel channel
  return res

subscribe :: Subscriber m -> ((m, Envelope) -> IO ()) -> IO ConsumerTag
subscribe (Subscriber (SubscriberSpec opts deserializer) channel) f =
  consumeMsgs
    channel
    (queueName opts)
    Ack
    (\(msg, env) -> do
       x <- deserializer $ msgBody msg
       f (x, env))

--
-- Well-known exchange/queue declarations
--
taskQueueName :: T.Text
taskQueueName = "tasks"

taskQueueOpts :: QueueOpts
taskQueueOpts = newQueue {queueName = taskQueueName}

taskErrorQueueName :: T.Text
taskErrorQueueName = "tasksErrors"

taskErrorQueueOpts :: QueueOpts
taskErrorQueueOpts = newQueue {queueName = taskErrorQueueName}

taskResultQueueName :: T.Text
taskResultQueueName = "tasksResults"

taskResultQueueOpts :: QueueOpts
taskResultQueueOpts = newQueue {queueName = taskResultQueueName}

declareStandardQueues :: Channel -> IO ()
declareStandardQueues channel =
  mapM_ (declareQueue channel) stdQueues
  where stdQueues = [taskQueueOpts, taskErrorQueueOpts, taskResultQueueOpts]

withTaskPublisher :: Connection -> (Publisher Task -> IO a) -> IO a
withTaskPublisher connection f = withPublisher connection spec $ \publisher -> do
  _ <- declareQueue (pubChan publisher) taskQueueOpts
  f publisher
  where
    spec =
      PublisherSpec
      { pubExchangeOpts = Nothing
      , pubKey = taskQueueName
      , pubSerializer = defaultSerializer
      , pubAwaitNanos = Just 1000000
      }

taskResultSubscriber :: Connection -> IO (Subscriber TaskResult)
taskResultSubscriber connection = openChannel connection >>= newSubscriber spec
  where
    spec =
      SubscriberSpec (newQueue {queueName = taskResultQueueName}) defaultDeserializer

