{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Module      : Main
-- Copyright   : (c) Joachim Fasting 2009
-- License     : GPL-2 (see COPYING)
--
-- Maintainer  : joachim.fasting@gmail.com
-- Stability   : unstable
-- Portability : not portable
--
-- A FUSE filesystem for the Music Player Daemon (MPD).
-- See README for more information.

module Main (main) where

import Control.Concurrent (forkIO)
import qualified Control.Concurrent.Chan as C
import qualified Control.Concurrent.MVar as C
import Control.Monad (liftM)
import Control.Monad.Error (catchError)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import System.FilePath ((</>), takeBaseName, takeDirectory, splitDirectories)
import qualified System.Fuse as F
import System.Posix hiding (createDirectory, rename)
import Network.MPD hiding (rename)
import Network.MPD.Core (close)
import qualified Network.MPD as M
import Prelude hiding (readFile, writeFile)

data Request
    = forall a. ReqSync (MPD a) (C.MVar (Response a))
    | ReqAsync (MPD ())
    | ReqDone

main :: IO ()
main = do
    chan  <- C.newChan
    mDone <- C.newEmptyMVar
    threadId <- forkIO (withMPD (mpdloop chan mDone) >> return ())
    F.fuseMain (operations chan) F.defaultExceptionHandler
    C.takeMVar mDone
    return ()

mpdloop :: C.Chan Request -> C.MVar () -> MPD ()
mpdloop chan mDone = loop
    where
        loop = liftIO (C.readChan chan) >>= go

        go (ReqSync action result) = do
            res <- catchError (Right `fmap` action) (return . Left)
            liftIO (C.putMVar result res) >> loop

        go (ReqAsync action) = do
            action >> loop

        go (ReqDone) = do
            close >> liftIO (C.putMVar mDone ())

--
-- FUSE operations.
--

operations :: C.Chan Request -> F.FuseOperations fh
operations chan = F.defaultFuseOps
    { F.fuseGetFileStat      = stat chan
    , F.fuseOpen             = openFile chan
    , F.fuseRead             = readFile chan
    , F.fuseWrite            = writeFile chan
    , F.fuseReadDirectory    = readDir chan
    , F.fuseOpenDirectory    = openDirectory chan
    , F.fuseCreateDirectory  = createDirectory chan
    , F.fuseRename           = rename chan
    -- Dummies to make FUSE happy.
    , F.fuseSetFileSize      = (\_ _   -> return F.eOK)
    , F.fuseSetFileTimes     = (\_ _ _ -> return F.eOK)
    , F.fuseSetFileMode      = (\_ _   -> return F.eOK)
    , F.fuseSetOwnerAndGroup = (\_ _ _ -> return F.eOK)
    }

openDirectory :: C.Chan Request -> FilePath -> IO F.Errno
openDirectory chan p = do
    putStrLn $ "OPEN DIRECTORY: " ++ p
    st <- stat chan p
    case st of
        Right st' -> case F.statEntryType st' of
                         F.Directory -> return F.eOK
                         _           -> return F.eNOTDIR
        _         -> return F.eNOENT

createDirectory :: C.Chan Request -> FilePath -> FileMode -> IO F.Errno
createDirectory chan p _ = do
    putStrLn $ "CREATE DIRECTORY: " ++ p
    case splitDirectories ("/" </> p) of
        ("/":"Playlists":plName:[]) ->
            either (const F.eNOENT) (const F.eOK) `fmap`
                fuseMPD chan (add_ plName "")

rename :: C.Chan Request -> FilePath -> FilePath -> IO F.Errno
rename chan p newName = do
    putStrLn $ "RENAME DIRECTORY: " ++ p ++ " " ++ newName
    return F.eOK

-- Implements the readdir(3) call.
readDir :: C.Chan Request -> FilePath
        -> IO (Either F.Errno [(FilePath, F.FileStat)])
readDir chan p = do
    putStrLn $ "READ DIRECTORY: " ++ p
    Right `liftM` getDirectoryContents chan p

-- Implements the open(3) call.
openFile :: C.Chan Request -> FilePath -> OpenMode -> OpenFileFlags
         -> IO (Either F.Errno fh)
openFile chan p _ _ = do
    putStrLn $ "OPEN FILE: " ++ p
    st <- stat chan p
    case st of
        Right st' -> case F.statEntryType st' of
                         F.RegularFile -> return $ Right undefined
                         _             -> return $ Left F.eNOENT
        _         -> return $ Left F.eNOENT

-- Implements the read(3) call.
-- XXX: needs cleanup
readFile :: C.Chan Request -> FilePath -> fh -> ByteCount -> FileOffset
         ->IO (Either F.Errno ByteString)
readFile chan p _ _ _ = do
    putStrLn $ "READ FILE " ++ p
    case splitDirectories ("/" </> p) of
        ("/":"Outputs":_:[]) -> readDeviceFile chan p
        ("/":"Stats":_:[])   -> readStatsFile chan p
        _                    -> return $ Left F.eNOENT -- this should be
                                                       -- handled by
                                                       -- openFile?

readDeviceFile chan p = fuseMPD chan $ do
   xs <- outputs
   case filter ((==) (takeDeviceID p) . dOutputID) xs of
       [d] -> return . B.pack $
              (if dOutputEnabled d then "1" else "0") ++ "\n"
       _   -> undefined -- assume openFile makes sure this will never happen

readStatsFile chan p = fuseMPD chan $
    case lookup (takeBaseName p) selectors of
        Just f -> (flip B.snoc '\n' . packInt . f) `liftM` stats
        _      -> undefined -- let's pretend openFile will prevent us from
                            -- going here
    where
        selectors = [("artists", stsArtists)
                    ,("albums", stsAlbums)
                    ,("playtime", stsPlaytime)
                    ,("songs", stsSongs)
                    ,("uptime", stsUptime)
                    ,("db_playtime", stsDbPlaytime)
                    ,("db_update", stsDbUpdate)]

-- Implements the pwrite(2) call.
-- XXX: needs cleanup
writeFile :: C.Chan Request -> FilePath -> fh -> ByteString -> FileOffset
          -> IO (Either F.Errno ByteCount)
writeFile chan p _ s _ = do
    putStrLn $ "WRITE FILE" ++ p

    r <- case splitDirectories ("/" </> p) of
             ("/":"Outputs":_:[]) -> writeDeviceFile chan p s
             _                    -> return $ Left F.eNOENT

    return $
           either Left (const $ Right . fromIntegral $ B.length s) r

writeDeviceFile chan p s = fuseMPD chan $ do
    let setState = case B.readInt s of Just (0, _) -> disableOutput
                                       _ -> enableOutput
    setState (takeDeviceID p)

-- Implements the stat(3) call.
stat :: C.Chan Request -> FilePath -> IO (Either F.Errno F.FileStat)
stat _ "/" = return $ Right directory
stat chan p = do
    putStrLn $ "STAT DIRECTORY: " ++ p
    cs <- getDirectoryContents chan (takeDirectory p)
    case lookup (takeBaseName p) cs of
        Just s  -> return $ Right s
        Nothing -> return $ Left F.eNOENT

--
-- File system description.
--

getDirectoryContents :: C.Chan Request -> FilePath
                     -> IO [(FilePath, F.FileStat)]
getDirectoryContents chan p = ioMPD chan $ do
    -- NOTE: we make sure that paths begin with a slash for convenience.
    case splitDirectories ("/" </> p) of
        ("/":[]) -> return $ dots ++ [("Music", directory)
                           ,("Outputs", directory)
                           ,("Playlists", directory)
                           ,("Status", directory)
                           ,("Stats", directory)]
        ("/":"Music":[]) -> return dots
        ("/":"Outputs":[]) -> do
            devs <- outputs
            return $ dots ++ map (\x -> (deviceFileName x, mkFileStat (B.pack "0"))) devs
        ("/":"Playlists":[]) -> do
            pls <- lsPlaylists
            return $ dots ++ map (\x -> (x, directory)) pls
        ("/":"Playlists":plName:[]) -> do
            pls <- lsPlaylists
            if plName `elem` pls
             then do songs <- listPlaylist plName
                     return $ dots ++ map (flip (,) regularFile) songs
             else fail ""
        ("/":"Status":[]) -> do
            st <- status
            return $ dots ++ [("state", mkFileStat (packInt $ stState st))
                   ,("volume", mkFileStat (packInt $ stVolume st))
                   ,("random_mode", mkFileStat (packInt $ stRandom st))
                   ,("playlist_version", mkFileStat (packInt $ stPlaylistVersion st))
                   ,("playlist_length", mkFileStat (packInt $ stPlaylistLength st))
                   ,("song_pos", mkFileStat (packInt $ stSongPos st))
                   ,("song_id", mkFileStat (packInt $ stSongID st))
                   ,("time", mkFileStat (packInt $ stTime st))
                   ,("bitrate", mkFileStat (packInt $ stBitrate st))
                   ,("crossfade", mkFileStat (packInt $ stXFadeWidth st))
                   ,("audio", mkFileStat (packInt $ stAudio st))
                   ,("updating_db", mkFileStat (packInt $ stUpdatingDb st))
                   ,("error", mkFileStat (B.pack $ stError st))]
        ("/":"Stats":[]) -> do
            sts <- stats
            return $ dots ++ [("artists", mkFileStat (packInt $ stsArtists sts))
                   ,("albums", mkFileStat (packInt $ stsAlbums sts))
                   ,("songs", mkFileStat (packInt $ stsSongs sts))
                   ,("uptime", mkFileStat (packInt $ stsUptime sts))
                   ,("playtime", mkFileStat (packInt $ stsPlaytime sts))
                   ,("db_playtime", mkFileStat (packInt $ stsDbPlaytime sts))
                   ,("db_update", mkFileStat (packInt $ stsDbUpdate sts))]
        _ -> fail "No such directory"
    where dots = [(".", directory), ("..", directory)]

-- Given a file content as a string, produce a file stat with appropriate size
-- and block information.
mkFileStat s = regularFile { F.statFileSize = fromIntegral len
                           , F.statBlocks   = fromIntegral blk }
    where
        len = B.length s + 1 -- remember space for trailing newline
        blk = (len `div` 4096) + 1

songFileStat :: Song -> F.FileStat
songFileStat sg = regularFile { F.statFileSize = fromIntegral $ sgLength sg }

directory, regularFile, emptyStat :: F.FileStat
directory = emptyStat
    { F.statEntryType = F.Directory
    , F.statFileSize = 4096
    , F.statBlocks = 1
    , F.statFileMode = foldr1 unionFileModes [ ownerReadMode, ownerExecuteMode ]
    }

regularFile = emptyStat
    { F.statEntryType = F.RegularFile
    , F.statFileMode = foldr1 unionFileModes [ ownerReadMode, ownerWriteMode ]
    }

emptyStat = F.FileStat
    { F.statEntryType = F.Unknown
    , F.statFileMode = ownerModes
    , F.statLinkCount = 0
    , F.statFileOwner = 0
    , F.statFileGroup = 0
    , F.statSpecialDeviceID = 0
    , F.statFileSize = 0
    , F.statBlocks = 0
    , F.statAccessTime = 0
    , F.statModificationTime = 0
    , F.statStatusChangeTime = 0
    }

--
-- Mapping MPD data structures to file system objects.
--

takeDeviceID :: FilePath -> Int
takeDeviceID = read . take 1 . takeBaseName

deviceFileName :: Device -> FilePath
deviceFileName (Device i n _) = show i ++ ":" ++ replace ' ' '_' n

songFileName :: Song -> FilePath
songFileName = undefined

--
-- Utilities.
--

-- Run an action in the MPD monad and lift the result into the FUSE
-- context.
fuseMPD :: C.Chan Request -> MPD a -> IO (Either F.Errno a)
fuseMPD chan x = do
    mResult <- C.newEmptyMVar
    C.writeChan chan $ ReqSync x mResult
    either (const $ Left F.eNOENT) Right `fmap` C.takeMVar mResult

-- Run an action in the MPD monad and lift the result into I/O.
ioMPD :: C.Chan Request -> MPD a -> IO a
ioMPD chan m = fuseMPD chan m
           >>= either (\(F.Errno x) -> fail (show x)) return

replace :: Eq a => a -> a -> [a] -> [a]
replace from to = map (\x -> if x == from then to else x)

packInt :: Show a => a -> ByteString
packInt = B.pack . show
