{-# LANGUAGE NoMonomorphismRestriction #-}

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

import Data.List (isPrefixOf)
import Control.Monad (join, liftM)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import System.FilePath ((</>), takeBaseName, takeDirectory, splitDirectories)
import System.Fuse
import System.Posix hiding (createDirectory, rename)
import Network.MPD hiding (rename)
import qualified Network.MPD as M
import Prelude hiding (readFile, writeFile)

main :: IO ()
main = fuseMain operations defaultExceptionHandler

--
-- FUSE operations.
--

operations :: FuseOperations fh
operations = defaultFuseOps
    { fuseGetFileStat = stat
    , fuseOpen = openFile
    , fuseRead = readFile
    , fuseWrite = writeFile
    , fuseReadDirectory = readDir
    , fuseOpenDirectory = openDirectory
    , fuseCreateDirectory = createDirectory
    , fuseRename = rename
    -- Dummies to make FUSE happy.
    , fuseSetFileSize = (\_ _ -> return eOK)
    , fuseSetFileTimes = (\_ _ _ -> return eOK)
    , fuseSetFileMode = (\_ _ -> return eOK)
    , fuseSetOwnerAndGroup = (\_ _ _ -> return eOK)
    }

openDirectory :: FilePath -> IO Errno
openDirectory p = do
    putStrLn $ "OPEN DIRECTORY: " ++ p
    st <- stat p
    case st of
        Right st' -> case statEntryType st' of
                         Directory -> return eOK
                         _         -> return eNOTDIR
        _         -> return eNOENT

createDirectory :: FilePath -> FileMode -> IO Errno
createDirectory p _ = do
    putStrLn $ "CREATE DIRECTORY: " ++ p
    return eOK

rename :: FilePath -> FilePath -> IO Errno
rename p newName = do
    putStrLn $ "RENAME DIRECTORY: " ++ p ++ " " ++ newName
    return eOK

-- Implements the readdir(3) call.
readDir :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
readDir p = do
    putStrLn $ "READ DIRECTORY: " ++ p
    Right `liftM` getDirectoryContents p

-- Implements the open(3) call.
openFile :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno fh)
openFile p _ _ = do
    putStrLn $ "OPEN FILE: " ++ p
    st <- stat p
    case st of
        Right st' -> case statEntryType st' of
                         RegularFile -> return $ Right undefined
                         _           -> return $ Left eNOENT
        _         -> return $ Left eNOENT

-- Implements the read(3) call.
-- XXX: needs cleanup
readFile :: FilePath -> fh -> ByteCount -> FileOffset
         ->IO (Either Errno ByteString)
readFile p _ _ _ = do
    putStrLn $ "READ FILE " ++ p
    r <- case splitDirectories ("/" </> p) of
             ("/":"Outputs":_:[]) -> readDeviceFile p
             ("/":"Stats":_:[])   -> readStatsFile p
             _                    -> fail "No such file"
    return $ mpdToErrno r

readDeviceFile p = withMPD $ do
   xs <- outputs
   case filter ((==) (takeDeviceID p) . dOutputID) xs of
       [d] -> return . B.pack $
              if dOutputEnabled d then "1" else "0" ++ "\n"

readStatsFile p = withMPD $ do
    case lookup (takeBaseName p) selectors of
        Just f  -> (flip B.snoc '\n' . packInt . f) `liftM` stats
        Nothing -> fail "No such file"
    where
        selectors = [("artists", stsArtists)
                    ,("albums", stsAlbums)
                    ,("songs", stsSongs)
                    ,("uptime", stsUptime)
                    ,("db_playtime", stsDbPlaytime)
                    ,("db_update", stsDbUpdate)]

-- Implements the pwrite(2) call.
-- XXX: needs cleanup
writeFile :: FilePath -> fh -> ByteString -> FileOffset -> IO (Either Errno ByteCount)
writeFile p _ s _ = do
    putStrLn $ "WRITE FILE" ++ p

    r <- case splitDirectories ("/" </> p) of
             ("/":"Outputs":_:[]) -> writeDeviceFile p s
             _                    -> fail "No such file"

    return $ either (const $ Left eNOENT)
                    (const $ Right . fromIntegral $ B.length s)
                    r

writeDeviceFile p s = withMPD $ do
    let setState = case B.readInt s of Just (0, _) -> disableOutput
                                       _ -> enableOutput
    setState (takeDeviceID p)

-- Implements the stat(3) call.
stat :: FilePath -> IO (Either Errno FileStat)
stat "/" = return $ Right directory
stat p = do
    putStrLn $ "STAT DIRECTORY: " ++ p
    cs <- getDirectoryContents (takeDirectory p)
    case lookup (takeBaseName p) cs of
        Just s  -> return $ Right s
        Nothing -> return $ Left eNOENT

--
-- File system description.
--

getDirectoryContents :: FilePath -> IO [(FilePath, FileStat)]
getDirectoryContents p = join . liftM fromMPD . withMPD $ do
    -- NOTE: we make sure that paths begin with a slash for convenience.
    case splitDirectories ("/" </> p) of
        ("/":[]) -> return $ dots ++ [("Music", directory)
                           ,("Outputs", directory)
                           ,("Playlists", directory)
                           ,("Status", directory)
                           ,("Stats", directory)]
        ("/":"Music":[]) -> do
            return dots
        ("/":"Outputs":[]) -> do
            devs <- outputs
            return $ dots ++ map (\x -> (deviceFileName x, mkFileStat (B.pack "0"))) devs
        ("/":"Playlists":[]) -> do
            pls <- lsPlaylists
            return $ dots ++ map (\x -> (x, directory)) pls
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
mkFileStat s = regularFile { statFileSize = fromIntegral len
                           , statBlocks   = fromIntegral blk }
    where
        len = B.length s + 1 -- remember space for trailing newline
        blk = (len `div` 4096) + 1

directory, regularFile, emptyStat :: FileStat
directory = emptyStat
    { statEntryType = Directory
    , statFileSize = 4096
    , statBlocks = 1
    , statFileMode = foldr1 unionFileModes [ ownerReadMode, ownerExecuteMode ]
    }

regularFile = emptyStat
    { statEntryType = RegularFile
    , statFileMode = foldr1 unionFileModes [ ownerReadMode, ownerWriteMode ]
    }

emptyStat = FileStat
    { statEntryType = Unknown
    , statFileMode = ownerModes
    , statLinkCount = 0
    , statFileOwner = 0
    , statFileGroup = 0
    , statSpecialDeviceID = 0
    , statFileSize = 0
    , statBlocks = 0
    , statAccessTime = 0
    , statModificationTime = 0
    , statStatusChangeTime = 0
    }

--
-- Mapping MPD data structures to file system objects.
--

takeDeviceID :: FilePath -> Int
takeDeviceID = read . take 1 . takeBaseName

deviceFileName :: Device -> FilePath
deviceFileName (Device i n _) = show i ++ ":" ++ replace ' ' '_' n

--
-- Utilities.
--

mpdToErrno :: Response a -> Either Errno a
mpdToErrno (Left _)  = Left eNOENT
mpdToErrno (Right r) = Right r

fromMPD :: Response a -> IO a
fromMPD = either (fail . show) return

replace :: Eq a => a -> a -> [a] -> [a]
replace from to = map (\x -> if x == from then to else x)

anyPrefix :: Eq a => [a] -> [[a]] -> Bool
anyPrefix s prefixes = any (`isPrefixOf` s) prefixes

packInt :: Show a => a -> ByteString
packInt = B.pack . show
