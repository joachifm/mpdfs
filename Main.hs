{- Layout

/Music/         The music database, where each song is represented as a file
                Seeking in a file will cause playback to seek
                Reading a file will append it to the playlist and play it back
                The size encodes the length in seconds
                Read-only
                Sub-folders contain different views of the database
    Genres      Songs organised by genre
    Artists     Songs organised by artist
    Albums      Songs organised by album

/Playlists/     Playlists, one directory per playlist
                Creating a directory creates a playlist with the same name
                Moving files to/from a playlist directory manipulates the playlist with the same name
    Current/    Contains the current playlist
                Deleting this directory clears the current playlist
/Outputs/       Output devices, one read-only file per device
/stats          A read-only file containing MPD statistics
/state          Current player state (one of: play, pause, stop)
                Writing to this file changes the player state
/volume         Current volume (absolute value)
                Writing to this file changes the current volume
/control        Execute commands by writing to this file

TODO:
- Modes should be set according to admin privileges
-}

import Data.List
import Foreign.C.Error
import System.FilePath
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import System.Posix

import Network.MPD
import System.Fuse

main :: IO ()
main = fuseMain mpdFSOps defaultExceptionHandler

mpdFSOps :: FuseOperations fh
mpdFSOps = defaultFuseOps
    { fuseGetFileStat = mpdGetFileStat
    , fuseRead = mpdRead
    , fuseWrite = mpdWrite
    , fuseOpen = mpdOpen
    , fuseReadDirectory = mpdReadDirectory
    , fuseOpenDirectory = mpdOpenDirectory
    , fuseCreateDirectory = mpdCreateDirectory
    , fuseRename = mpdRename
    , fuseGetFileSystemStats = mpdGetFileSystemStats
    }

mpdReadDirectory :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
mpdReadDirectory "/" = return $ Right
    [(".", mpdDirEntry)
    ,("..", mpdDirEntry)
    ,("Outputs", mpdDirEntry)
    ,("stats", mpdFileEntry)]
mpdReadDirectory "Outputs" = do
    r <- withMPD outputs
    case r of
        Right os -> return $ Right []
        Left _   -> return $ Left eNOENT
mpdReadDirectory "/stats" = return $ Left eNOTDIR
mpdReadDirectory _ = return $ Left eNOENT

mpdOpenDirectory :: FilePath -> IO Errno
mpdOpenDirectory p
    | p `elem` ["/", "/Outputs"] = return $ eOK
mpdOpenDirectory "/stats" = return $ eNOTDIR
mpdOpenDirectory _ = return $ eNOENT

mpdCreateDirectory :: FilePath -> FileMode -> IO Errno
mpdCreateDirectory _ _ = return eOK

mpdRename :: FilePath -> FilePath -> IO Errno
mpdRename _ _ = return eOK

mpdOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno fh)
mpdOpen "/stats" ReadOnly _ = return $ Right undefined
mpdOpen "/stats" _ _        = return $ Left eACCES
mpdOpen _ _ _               = return $ Left eNOENT

mpdRead :: FilePath -> fh -> ByteCount -> FileOffset -> IO (Either Errno ByteString)
mpdRead "/stats" _ _ _ = do
    r <- withMPD stats
    case r of
        Right sts -> do
            return . Right . B.pack $ unlines
                [ "artists: " ++ show (stsArtists sts)
                , "albums: " ++ show (stsAlbums sts)
                , "songs: " ++ show (stsSongs sts)
                , "uptime: " ++ show (stsUptime sts)
                , "playtime: " ++ show (stsPlaytime sts)
                , "db_playtime: " ++ show (stsDbPlaytime sts)
                , "db_update: " ++ show (stsDbUpdate sts)
                ]
        Left e    -> do
            print e
            return $ Left eNOENT
mpdRead _ _ _ _ = return $ Left eNOENT

mpdWrite :: FilePath -> fh -> ByteString -> FileOffset -> IO (Either Errno ByteCount)
mpdWrite _ _ _ _ = return $ Left eNOENT

mpdGetFileStat :: FilePath -> IO (Either Errno FileStat)
mpdGetFileStat "/" = do
    ctx <- getFuseContext
    return . Right $ mpdDirEntry
        { statFileOwner = fuseCtxUserID ctx
        , statFileGroup = fuseCtxGroupID ctx
        }
mpdGetFileStat "/Outputs" = do
    ctx <- getFuseContext
    return . Right $ mpdDirEntry
        { statFileOwner = fuseCtxUserID ctx
        , statFileGroup = fuseCtxGroupID ctx
        }
mpdGetFileStat "/stats" = do
    ctx <- getFuseContext
    return . Right $ mpdFileEntry
        { statFileOwner = fuseCtxUserID ctx
        , statFileGroup = fuseCtxGroupID ctx
        , statFileSize  = 4096
        , statFileMode  = allReadMode
        }
mpdGetFileStat _ = return $ Left eNOENT

allReadMode    = foldr1 unionFileModes [ ownerReadMode, groupReadMode, otherReadMode ]
allExecuteMode = foldr1 unionFileModes [ ownerExecuteMode, groupExecuteMode, otherExecuteMode ]
allWriteMode   = foldr1 unionFileModes [ ownerWriteMode, groupWriteMode, otherWriteMode ]

mpdDirEntry = emptyFileStat
    { statEntryType = Directory
    , statFileSize = 4096
    , statBlocks = 1
    }

mpdFileEntry = emptyFileStat
    { statEntryType = RegularFile
    }

emptyFileStat = FileStat
    { statEntryType = Unknown
    , statLinkCount = 0
    , statFileMode = foldr1 unionFileModes [ allReadMode, allExecuteMode, allWriteMode ]
    , statFileOwner = 0
    , statFileGroup = 0
    , statSpecialDeviceID = 0
    , statFileSize = 0
    , statBlocks = 0
    , statAccessTime = 0
    , statModificationTime = 0
    , statStatusChangeTime = 0
    }

mpdGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
mpdGetFileSystemStats _ = return $ Left eOK
