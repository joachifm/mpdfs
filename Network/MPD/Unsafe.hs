{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances #-}

-- | Module   : Network.MPD.Unsafe
-- Copyright  : (c) Joachim Fasting 2009
-- License    : LGPL (see LICENSE)
-- Maintainer : joachim.fasting@gmail.com
-- Stability  : unstable
--
-- Makes puppies cry.
-- Intenionally left undocumented, please read the source before using.
--

module Network.MPD.Unsafe (UnsafeMPD(..), runUnsafeMPD, MonadMPD(..)) where

import Network.MPD.Core hiding (io)
import Network

import Control.Exception (IOException, catch)
import Prelude hiding (catch)
import Control.Monad (liftM)
import Control.Monad.Error (ErrorT(..), MonadError(..))
import Control.Monad.Trans
import Data.List
import Data.IORef
import System.IO.Unsafe
import System.IO

--
-- Configuration
--

port = 6600
host = "localhost"

--
-- Primitives
--

connection :: IORef (Maybe Handle)
connection = unsafePerformIO (newIORef Nothing)
{-# NOINLINE connection #-}

access :: IO (Maybe Handle)
access = readIORef connection

set :: (Maybe Handle) -> IO ()
set = writeIORef connection

put :: String -> IO ()
put s = withConnection (\h -> hPutStrLn h s >> hFlush h)

get :: IO String
get = withConnection $ \h -> do
    let slurp acc = do
        l <- hGetLine h
        if "OK" `isPrefixOf` l || "ACK" `isPrefixOf` l
            then return . unlines $ reverse (l:acc)
            else slurp (l:acc)
    slurp []

--
-- Convenience
--

whenJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM f m = maybe (return ()) m =<< f

whenConnected :: (Handle -> IO ()) -> IO ()
whenConnected m = whenJustM access m

withConnection :: (Handle -> IO a) -> IO a
withConnection m = maybe (fail "No connection") m =<< access

--
-- API
--

newtype UnsafeMPD a = UnsafeMPD { unsafeMPD :: ErrorT MPDError IO a }
    deriving (Monad, MonadIO, MonadError MPDError)

instance MonadMPD UnsafeMPD where
    send  = io . sendConnection
    open  = io openConnection
    close = io closeConnection
    getPassword = throwError (Custom "Oops")

runUnsafeMPD :: UnsafeMPD a -> IO (Either MPDError a)
runUnsafeMPD = runErrorT . unsafeMPD

io :: IO a -> UnsafeMPD a
io m =
    maybe (throwError $ Custom "Awww, poor puppy...") return =<< liftIO (maybeIO m)

maybeIO :: IO a -> IO (Maybe a)
maybeIO m = liftM Just m `catch` ((\_ -> return Nothing) :: IOException -> IO (Maybe a))

sendConnection :: String -> IO String
sendConnection s = put s >> get

closeConnection :: IO ()
closeConnection = withConnection $ \h -> do
    put "close"
    hClose h
    set Nothing

openConnection :: IO ()
openConnection = do
    whenConnected $ fail "Already connected."
    h <- connectTo host (PortNumber $ fromInteger port)
    set (Just h)
    -- Read OK message
    get
    return ()
