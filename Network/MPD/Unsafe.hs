{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Module   : Network.MPD.Unsafe
-- Copyright  : (c) Joachim Fasting 2009
-- License    : LGPL (see LICENSE)
-- Maintainer : joachim.fasting@gmail.com
-- Stability  : unstable
--
-- Makes puppies cry.
-- Intentionally left undocumented, please read the source before using.
--

module Network.MPD.Unsafe (MonadMPD(..), UnsafeMPD(..), unsafeMPD) where

import Network.MPD.Core (MonadMPD, MPDError(..), Response)
import qualified Network.MPD.Core as M

import Control.Concurrent.MVar
import Control.Exception
import Prelude hiding (catch)
import Control.Monad.Error
import Control.Monad.Trans
import Data.IORef
import Data.List
import Data.Maybe
import Network
import System.IO.Unsafe
import System.IO

--
-- MonadMPD instance
--

newtype UnsafeMPD a = UnsafeMPD { unMPD :: ErrorT MPDError IO a }
    deriving (Monad, MonadIO, MonadError MPDError)

instance MonadMPD UnsafeMPD where
    send = io . send
    open = io (open "localhost" 6600)
    close = io close
    getPassword = throwError (Custom "Not supported.")

unsafeMPD :: UnsafeMPD a -> IO (Response a)
unsafeMPD = runErrorT . unMPD

io :: IO a -> UnsafeMPD a
io m = maybe (throwError $ Custom "") return =<< liftIO (maybeIO m)

maybeIO :: IO a -> IO (Maybe a)
maybeIO m = liftM Just m `catch` ((\_ -> return Nothing) :: IOException -> IO (Maybe a))

--
-- API
--

open :: String -> Int -> IO ()
open host port = do
    isConnected >>= flip when (fail "Already connected.")
    h <- connectTo host (PortNumber $ fromIntegral port)
    set h
    -- Read OK
    hGet h
    return ()

close :: IO ()
close = ask >>= maybe (return ()) hClose

send :: String -> IO String
send s = ask >>= maybe (fail "No connection.") (\h -> hPut h s >> hGet h)

isConnected :: IO Bool
isConnected = maybe (return False) hIsOpen =<< ask

--
-- Private
--

global :: IORef (MVar Handle)
global = unsafePerformIO (newEmptyMVar >>= newIORef)
{-# NOINLINE global #-}

-- Return a handle if there is one.
ask :: IO (Maybe Handle)
ask = do
    gv      <- readIORef global
    isEmpty <- isEmptyMVar gv
    if isEmpty
       then return Nothing
       else Just `liftM` readMVar gv

-- Overwrite handle.
set :: Handle -> IO ()
set h = newMVar h >>= writeIORef global

hPut :: Handle -> String -> IO ()
hPut h s = hPutStrLn h s >> hFlush h

hGet :: Handle -> IO String
hGet h = let slurp acc = do
                 l <- hGetLine h
                 if "OK" `isPrefixOf` l || "ACK" `isPrefixOf` l
                    then return . unlines $ reverse (l:acc)
                    else slurp (l:acc)
         in slurp []
