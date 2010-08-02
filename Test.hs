module Main (main) where

import Test.Framework
import qualified Test.Framework as TF
import Test.Framework.Providers.HUnit
import Test.HUnit

import Control.Concurrent (forkOS)
import Control.Concurrent.MVar (MVar, newEmptyMVar, readMVar, putMVar)
import Control.Exception (finally)
import Control.Monad.Trans (liftIO)

import qualified Data.Binary as B

import qualified System.ZMQ as ZMQ

import Control.Monad.BinaryProtocol.ZMQ
    (BinaryProtocol, runProtocol, send, receive, flush)

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests =
    [ testGroup "unidirectional communications"
      [ testCase "send unit" testSendUnit
      , testCase "send number" testSendNumber
      , testCase "send list of numbers" testSendListOfNumbers
      ]

    , testGroup "bidirectional communications"
      [ testCase "addition" testAddition
      ]
    ]

makeChannels :: ZMQ.Context -> String -> IO (ZMQ.Socket ZMQ.Up,
                    ZMQ.Socket ZMQ.Down)
makeChannels ctx address = do
    chan1 <- ZMQ.socket ctx ZMQ.Up
    chan2 <- ZMQ.socket ctx ZMQ.Down

    ZMQ.bind chan1 address
    ZMQ.connect chan2 address

    return (chan1, chan2)

makeSendTest :: (B.Binary a, Eq a, Show a) => a -> IO ()
makeSendTest value = do
    ctx <- ZMQ.init 1
    (chan_in, chan_out) <- makeChannels ctx "inproc://pipe"

    result <- runProtocol actions chan_in chan_out `finally` do
        ZMQ.close chan_out
        ZMQ.close chan_in
        ZMQ.term ctx

    assertEqual "Was the correct value received?" value result
  where actions = do
            send value
            flush
            receive

testSendUnit :: IO ()
testSendUnit = makeSendTest ()

testSendNumber :: IO ()
testSendNumber = makeSendTest (3 :: Int)

testSendListOfNumbers :: IO ()
testSendListOfNumbers = makeSendTest [3 :: Int, 4, 5, 6]


makeExchangeTest :: (B.Binary a, Show a, Eq a) =>
    a ->
    (MVar a -> BinaryProtocol ZMQ.Up ZMQ.Down ()) ->
    (MVar a -> BinaryProtocol ZMQ.Up ZMQ.Down ()) ->
    IO ()
makeExchangeTest correct_result protocol1 protocol2 = do
    resultMVar <- newEmptyMVar

    ctx <- ZMQ.init 1

    lock1 <- newEmptyMVar
    lock2 <- newEmptyMVar

    -- ZeroMQ sockets can only be used in the thread which created them.
    -- We need some magic to get this right.
    f $ forkOS $ runProtocol' address1 address2 ctx lock1 lock2
        (protocol1 resultMVar)
    f $ forkOS $ runProtocol' address2 address1 ctx lock2 lock1
        (protocol2 resultMVar)

    result <- readMVar resultMVar `finally` ZMQ.term ctx

    assertEqual "Was the correct result computed?" correct_result result

  where address1 = "inproc://pipe1"
        address2 = "inproc://pipe2"

        f :: IO a -> IO ()
        f a = a >> return ()

        runProtocol' :: String -> String -> ZMQ.Context ->
            MVar () -> MVar () ->
            BinaryProtocol ZMQ.Up ZMQ.Down () -> IO ()
        runProtocol' a1 a2 ctx l1 l2 p = do
            chan_in <- ZMQ.socket ctx ZMQ.Up
            chan_out <- ZMQ.socket ctx ZMQ.Down

            ZMQ.bind chan_in a1
            putMVar l1 ()

            f $ readMVar l2
            ZMQ.connect chan_out a2

            runProtocol p chan_in chan_out `finally` do
                ZMQ.close chan_in
                ZMQ.close chan_out


testAddition :: IO ()
testAddition =
    makeExchangeTest (3 :: Int)
        (\resultMVar -> do
             send (1 :: Int)
             flush
             receive >>= liftIO . putMVar resultMVar
        )
        (\_ -> do
             a <- receive
             send (a + (2 :: Int))
             flush
        )
