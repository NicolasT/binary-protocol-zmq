{-# LANGUAGE GeneralizedNewtypeDeriving #-}

----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.BinaryProtocol.ZMQ
-- Copyright   :  (c) 2010 Nicolas Trangez
-- License     :  BSD3
-- 
-- Maintainer  :  Nicolas Trangez <eikke@eikke.com>
-- Stability   :  experimental
--
-- Monad to ease implementing a binary network protocol over ZeroMQ
--
-----------------------------------------------------------------------------

module Control.Monad.BinaryProtocol.ZMQ
    (
      BinaryProtocol
    , runProtocol
    , receive
    , receive'
    , send
    , send'
    , flush
    ) where

import qualified Control.Monad.Reader as R
import qualified Control.Monad.Trans as T

import qualified Data.Binary as B
import qualified Data.Binary.Get as BG

import qualified Data.ByteString.Lazy as LB

import qualified System.ZMQ as ZMQ

-- | Action type definition. @a@ is the type of the reader ZeroMQ socket,
--   @b@ is the type of the writer ZeroMQ socket, and @c@ is the return type of
--   the action.
newtype BinaryProtocol a b c = BP {
    runBP :: R.ReaderT (ZMQ.Socket a, ZMQ.Socket b) IO c
} deriving (Monad, T.MonadIO, R.MonadReader (ZMQ.Socket a, ZMQ.Socket b))

-- | Take a @BinaryProtocol@ action and run it on the given ZeroMQ sockets for
--   respectively reading and writing. The two given handles are allowed to be
--   the same if the same handle is used for reading and writing.
runProtocol :: BinaryProtocol a b c -> ZMQ.Socket a -> ZMQ.Socket b -> IO c
runProtocol p a b = R.runReaderT (runBP p) (a, b)

-- | Read in a value of type @c@ from the connection; @c@ must be an instance
--   of the @Binary@ class. This is a wrapper around @receive'@, not passing
--   any flags.
receive :: B.Binary c => BinaryProtocol a b c
receive = receive' []

-- | Read in a value of type @c@ from the connection; @c@ must be an instance
--   of the @Binary@ class. A list of @Flag@s can be given.
receive' :: B.Binary c => [ZMQ.Flag] -> BinaryProtocol a b c
receive' flags =
    R.ask >>= \(sock, _) ->
    T.liftIO $ ZMQ.receive sock flags >>= \msg ->
    return $ BG.runGet B.get $ LB.fromChunks [msg]

-- | Send a value of type @c@ down the connection; @c@ must be an instance of
--   the @Binary@ class. This is a wrapper aroung @send'@, not passing any
--   flags.
send :: B.Binary c => c -> BinaryProtocol a b ()
send = send' []

-- | Send a value of type @c@ down the connection; @c@ must be an instance of
--   the @Binary@ class. A list of @Flag@s can be given.
send' :: B.Binary c => [ZMQ.Flag] -> c -> BinaryProtocol a b ()
send' flags value =
    R.ask >>= \(_, sock) ->
    T.liftIO $ ZMQ.send' sock msg flags
  where msg = B.encode value

-- | Flush connections
--
--   Note: this is a no-op, provided for API compatibility with the
--   @Control.Monad.BinaryProtocol@ package.
flush :: BinaryProtocol a b ()
flush = return ()
