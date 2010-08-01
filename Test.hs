module Main (main) where

import Test.Framework
import qualified Test.Framework as TF
import Test.Framework.Providers.HUnit
import Test.HUnit

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, readMVar, putMVar)
import Control.Exception (finally)
import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)

import System.IO (hPutStrLn, stderr)

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
    (chan_in1, chan_out2) <- makeChannels ctx address1
    (chan_in2, chan_out1) <- makeChannels ctx address2

    forkIO $ runProtocol (protocol1 resultMVar) chan_in1 chan_out1
    forkIO $ runProtocol (protocol2 resultMVar) chan_in2 chan_out2

    result <- readMVar resultMVar `finally` do
        forM_ [chan_in1, chan_in2] ZMQ.close
        forM_ [chan_out1, chan_out2] ZMQ.close
        ZMQ.term ctx

    assertEqual "Was the correct result computed?" correct_result result

  where address1 = "inproc://pipe1"
        address2 = "inproc://pipe2"

testAddition :: IO ()
testAddition = do
    hPutStrLn stderr
        "Warning: this test locks up sometimes, needs investigation"
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
