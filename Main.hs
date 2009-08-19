import Data.List
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
    -- Dummies to make FUSE happy.
    , fuseSetFileSize = (\_ _ -> return eOK)
    , fuseSetFileTimes = (\_ _ _ -> return eOK)
    , fuseSetFileMode = (\_ _ -> return eOK)
    , fuseSetOwnerAndGroup = (\_ _ _ -> return eOK)
    }

-- Determines what is returned by the readdir(3) call.
--
-- To add a new MPDFS directory make sure it has an entry here, in
-- 'mpdOpenDirectory' and in 'mpdGetFileStat'.
mpdReadDirectory :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
mpdReadDirectory "/" = return $ Right
    [(".", mpdDirectory)
    ,("..", mpdDirectory)
    ,("Outputs", mpdDirectory)
    ,("Stats", mpdDirectory)]
mpdReadDirectory "/Outputs" = do
    r <- withMPD outputs
    case r of
        Right os -> return . Right $
            [(".", mpdDirectory)
            ,("..", mpdDirectory)]
            ++ map (\x -> (outputFileName x, mpdWritableFile)) os
        Left e   -> do
               print e -- debug message, visible when mounted with '-d'
               return $ Left eNOENT
mpdReadDirectory "/Stats" = do
    r <- withMPD ping
    case r of
        Right _ -> return . Right $
           [(".", mpdDirectory)
           ,("..", mpdDirectory)
           ,("artists", mpdReadOnlyFile)
           ,("albums", mpdReadOnlyFile)
           ,("songs", mpdReadOnlyFile)
           ,("uptime", mpdReadOnlyFile)
           ,("playtime", mpdReadOnlyFile)
           ,("db_playtime", mpdReadOnlyFile)
           ,("db_update", mpdReadOnlyFile)]
        Left e -> do
               print e
               return $ Left eNOENT
mpdReadDirectory _ = return $ Left eNOENT

-- Determines what happens when someone requests to open a directory
-- in the file system.
mpdOpenDirectory :: FilePath -> IO Errno
mpdOpenDirectory p
    | p `elem` ["/", "/Outputs", "/Stats"] = return eOK
mpdOpenDirectory _ = return eNOENT

mpdCreateDirectory :: FilePath -> FileMode -> IO Errno
mpdCreateDirectory _ _ = return eOK

mpdRename :: FilePath -> FilePath -> IO Errno
mpdRename _ _ = return eOK

-- Determines what is returned by the open(3) call.
--
-- To add a new file to MPDFS make sure it has an entry here, in 'mpdRead',
--'mpdWrite', 'mpdOpen', and 'mpdGetFileStat'.
mpdRead :: FilePath -> fh -> ByteCount -> FileOffset -> IO (Either Errno ByteString)
mpdRead p _ _ _
    | "/Outputs" `isPrefixOf` p = do
    r <- withMPD outputs
    case r of
        Right os -> do
            let [d] = filter ((==) (takeOutputID p) . dOutputID) os
            print d
            return . Right $ B.pack ((if dOutputEnabled d then "1" else "0") ++ "\n")
        Left e -> do
            print e
            return $ Left eNOENT
    | "/Stats" `isPrefixOf` p = do
    let fileName = takeBaseName p
        field    = case fileName of
                       "artists"     -> stsArtists
                       "albums"      -> stsAlbums
                       "songs"       -> stsSongs
                       "uptime"      -> stsUptime
                       "playtime"    -> stsPlaytime
                       "db_playtime" -> stsDbPlaytime
                       "db_update"   -> stsDbUpdate
    r <- withMPD stats
    case r of
        Right sts -> return . Right . B.pack $ show (field sts) ++ "\n"
        Left e -> do
               print e
               return $ Left eNOENT

mpdRead _ _ _ _ = return $ Left eNOENT

-- Implements the pwrite(2) call.
mpdWrite :: FilePath -> fh -> ByteString -> FileOffset -> IO (Either Errno ByteCount)
mpdWrite p _ s _
    | "/Outputs" `isPrefixOf` p = do
    let newState = case B.unpack s of "0\n" -> disableOutput
                                      _     -> enableOutput
    print s
    print p
    r <- withMPD $ newState (takeOutputID p)
    case r of
        Right _ -> return $ Right (fromIntegral $ B.length s)
        Left e  -> do
            print e
            return $ Left eNOENT

mpdWrite _ _ _ _ = return $ Left eNOENT

mpdOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno fh)
mpdOpen p _ _
    | "/Outputs" `isPrefixOf` p = return $ Right undefined
    | "/Stats" `isPrefixOf` p   = return $ Right undefined
mpdOpen _ _ _               = return $ Left eNOENT

-- Implements the stat(3) call.
--
-- Remember that read(3) will not read more than 'statFileSize'.
mpdGetFileStat :: FilePath -> IO (Either Errno FileStat)
mpdGetFileStat "/" = do
    ctx <- getFuseContext
    return . Right $ mpdDirectory
        { statFileOwner = fuseCtxUserID ctx
        , statFileGroup = fuseCtxGroupID ctx
        }
mpdGetFileStat "/Outputs" = do
    ctx <- getFuseContext
    return . Right $ mpdDirectory
        { statFileOwner = fuseCtxUserID ctx
        , statFileGroup = fuseCtxGroupID ctx
        }
mpdGetFileStat "/Stats" = do
    ctx <- getFuseContext
    return . Right $ mpdDirectory
        { statFileOwner = fuseCtxUserID ctx
        , statFileGroup = fuseCtxGroupID ctx
        }
mpdGetFileStat p
    | "/Outputs" `isPrefixOf` p = do
    ctx <- getFuseContext
    return . Right $ mpdWritableFile
        { statFileOwner = fuseCtxUserID ctx
        , statFileGroup = fuseCtxGroupID ctx
        , statFileSize  = 4096
        }
    | "/Stats" `isPrefixOf` p = do
    ctx <- getFuseContext
    return . Right $ mpdWritableFile
        { statFileOwner = fuseCtxUserID ctx
        , statFileGroup = fuseCtxGroupID ctx
        -- XXX: we should use the actual size of the contents here
        , statFileSize  = 4096
        }
mpdGetFileStat _ = return $ Left eNOENT

mpdDirectory = emptyFileStat
    { statEntryType = Directory
    , statFileSize = 4096
    , statBlocks = 1
    }

mpdWritableFile = mpdReadOnlyFile `appendMode` allWriteMode

mpdReadOnlyFile = emptyFileStat
    { statEntryType = RegularFile
    , statFileMode = allReadMode
    }

emptyFileStat = FileStat
    { statEntryType = Unknown
    , statLinkCount = 0
    , statFileMode = 0
    , statFileOwner = 0
    , statFileGroup = 0
    , statSpecialDeviceID = 0
    , statFileSize = 0
    , statBlocks = 0
    , statAccessTime = 0
    , statModificationTime = 0
    , statStatusChangeTime = 0
    }

appendMode fstat mode =
    fstat { statFileMode = combineModes [ mode, statFileMode fstat ] }

allReadMode    = combineModes [ ownerReadMode, groupReadMode, otherReadMode ]
allExecuteMode = combineModes [ ownerExecuteMode, groupExecuteMode, otherExecuteMode ]
allWriteMode   = combineModes [ ownerWriteMode, groupWriteMode, otherWriteMode ]

combineModes = foldr1 unionFileModes

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
