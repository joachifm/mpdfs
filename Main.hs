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

--
-- File system operations.
--

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

-- Determines what is returned by the readdir(3) call.
--
-- To add a new MPDFS directory make sure it has an entry here, in
-- 'mpdOpenDirectory' and in 'mpdGetFileStat'.
mpdReadDirectory :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
mpdReadDirectory "/" = return $ Right
    [(".", mpdDirEntry)
    ,("..", mpdDirEntry)
    ,("Outputs", mpdDirEntry)
    ,("stats", mpdFileEntry)]
mpdReadDirectory "/Outputs" = do
    r <- withMPD outputs
    case r of
        Right os -> return . Right $
            [(".", mpdDirEntry)
            ,("..", mpdDirEntry)]
            ++ map (\x -> (outputFileName x, mpdFileEntry)) os
        Left e   -> do
               print e -- debug message, visible when mounted with '-d'
               return $ Left eNOENT

mpdReadDirectory "/stats" = return $ Left eNOTDIR
mpdReadDirectory _ = return $ Left eNOENT

-- Determines what happens when someone requests to open a directory
-- in the file system.
mpdOpenDirectory :: FilePath -> IO Errno
mpdOpenDirectory p
    | p `elem` ["/", "/Outputs"] = return $ eOK
mpdOpenDirectory "/stats" = return $ eNOTDIR
mpdOpenDirectory _ = return $ eNOENT

mpdCreateDirectory :: FilePath -> FileMode -> IO Errno
mpdCreateDirectory _ _ = return eOK

mpdRename :: FilePath -> FilePath -> IO Errno
mpdRename _ _ = return eOK

-- Determines what is returned by the open(3) call.
--
-- To add a new file to MPDFS make sure it has an entry here, in 'mpdRead',
--'mpdWrite', 'mpdOpen', and 'mpdGetFileStat'.
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
mpdRead p _ _ _
    | "/Outputs" `isPrefixOf` p = do
    let fileName = takeBaseName p
        outputID = read $ take 1 fileName
    r <- withMPD outputs
    case r of
        Right os -> do
            let [d] = filter ((==) outputID . dOutputID) os
            return . Right $ B.pack ((if dOutputEnabled d then "1" else "0") ++ "\n")
        Left e -> do
            print e
            return $ Left eNOENT

mpdRead _ _ _ _ = return $ Left eNOENT

mpdWrite :: FilePath -> fh -> ByteString -> FileOffset -> IO (Either Errno ByteCount)
mpdWrite p _ s _
    | "/Outputs" `isPrefixOf` p = do
    let newState = case B.unpack s of "0" -> disableOutput
                                      _   -> enableOutput
    r <- withMPD $ newState (takeOutputID p)
    case r of
        Right _ -> return $ Right 1
        Left e  -> do
            print e
            return $ Left eNOENT

mpdWrite _ _ _ _ = return $ Left eNOENT

mpdOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno fh)
mpdOpen "/stats" ReadOnly _ = return $ Right undefined
mpdOpen "/stats" _ _        = return $ Left eACCES
mpdOpen p _ _
    | "/Outputs" `isPrefixOf` p = return $ Right undefined
mpdOpen _ _ _               = return $ Left eNOENT

-- Implements the stat(3) call.
--
-- Remember that read(3) will not read more than 'statFileSize'.
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
mpdGetFileStat p
    | "/Outputs" `isPrefixOf` p = do
    ctx <- getFuseContext
    return . Right $ mpdFileEntry
        { statFileOwner = fuseCtxUserID ctx
        , statFileGroup = fuseCtxGroupID ctx
        , statFileSize  = 4096
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

--
-- Mapping MPD data structures to file system objects.
--

takeOutputID :: FilePath -> Int
takeOutputID = read . take 1 . takeBaseName

outputFileName :: Device -> FilePath
outputFileName (Device i n _) = show i ++ ":" ++ replace ' ' '_' n

--
-- Utilities.
--

replace :: Eq a => a -> a -> [a] -> [a]
replace e with = map (\x -> if x == e then with else x)
