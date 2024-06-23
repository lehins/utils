{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ < 800
main :: IO ()
main = do
  let (ghcMaj, ghcMin) = divMod (__GLASGOW_HASKELL__ :: Int) 100
      ghcVer = show ghcMaj ++ "." ++ show ghcMin
  putStrLn $ "GHC version: " ++ ghcVer ++ " is not supported by git-modtime.hs script."
#else
import Data.List
import Data.Maybe
import Data.Either (partitionEithers)
import Data.Char (isSpace)
import Control.Monad
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import System.FilePath (takeDirectory, (</>))
import System.Directory
  (setModificationTime, doesFileExist, createDirectoryIfMissing,
   doesDirectoryExist, getAppUserDataDirectory)
import System.Environment (getArgs)
import System.Process (readProcess)
import System.IO (IOMode(WriteMode), withFile, hPutStrLn, stderr)
import qualified Data.Map.Strict as Map


prefix :: String
prefix = "<git-modtime.hs> "

report :: String -> IO ()
report msg = hPutStrLn stderr $ prefix ++ msg

restoreFileModtime :: String -> FilePath -> IO ()
restoreFileModtime rev fp = do
  let iso8601 = "%Y-%m-%dT%H:%M:%S%Z"
  modTimeStr <- readProcess "git" ["log", "--pretty=format:%cI", "-1", rev, "--", fp] ""
  modTime <- parseTimeM True defaultTimeLocale iso8601 modTimeStr
  setModificationTime fp modTime
  report $ "[" ++ modTimeStr ++ "] " ++ fp

toFlags :: FilePath -> [String] -> Map.Map String String
toFlags baseDir =
  checkFlags . Map.fromList . uncurry zip . foldr (\x (l, r) -> (x : r, l)) ([], [])
  where
    checkFlags flags = do
      let unknownFlags = Map.keys (flags Map.\\ defaultFlags)
      if not (null unknownFlags)
        then error $ prefix ++ "Unknown flags: " ++ intercalate ", " unknownFlags
        else Map.union flags defaultFlags
    defaultFlags = Map.fromList [("-f", baseDir </> "tree-contents.txt")]

-- | Overwrites the file with contents hashes and return only the names for unchanged
-- files.
checkUnchanged :: FilePath -> [FilePath] -> IO [FilePath]
checkUnchanged contentsFilePath filePaths = do
  let dir = takeDirectory contentsFilePath
  directoryExists <- doesDirectoryExist dir
  unless directoryExists $
    report $ "Directory " ++ show dir ++ " does not exist, creating it."
  createDirectoryIfMissing True dir
  contentsFileExists <- doesFileExist contentsFilePath
  oldHashesMap <-
    if contentsFileExists
      then Map.fromList . catMaybes <$>
           (mapM parseLine . lines =<< readFile contentsFilePath)
      else mempty <$
           report
             ("Previous content hashes file was not found, will create it: " ++
              contentsFilePath)
  changedFiles <-
    withFile contentsFilePath WriteMode $ \hdl ->
      forM filePaths $ \fp -> do
        isDirectory <- doesDirectoryExist fp
        if isDirectory
          then pure $ Right fp
          else do
          hash <- filter (not . isSpace) <$> readProcess "git" ["hash-object", fp] ""
          hPutStrLn hdl (hash ++ " " ++ fp)
          pure $ case Map.lookup fp oldHashesMap of
            Just oldHash | oldHash == hash -> Right fp
            Just _ -> Left fp
            Nothing -> Left fp
  let (changed, unchanged) = partitionEithers changedFiles
  unless (null changed) $ do
    let sep = "\n  * "
    report $ "Previously not seen or changed files:" ++ sep ++ intercalate sep changed
  pure unchanged
  where
    parseLine ln =
      case span (/= ' ') ln of
        (hash, ' ':fp) -> pure $ Just (fp, hash)
        _ -> Nothing <$ report ("Invalid format in contents cache file: " ++ ln)



-- | Usage:
--
-- stack runghc -- git-modtime.hs [REV] [-f CONTENTS_FILE]
--
-- * REV - optional sha of a commit. Default is: HEAD
--
-- * CONTENTS_FILE - path to a file that will contain hashes of file contents from the
--   repo. Default is: ~/.stack/tree-contents.txt
--
main :: IO ()
main = do
  args <- getArgs
  appDir <- getAppUserDataDirectory "stack"
  let (rev, flags) =
        case args of
          [] -> ("HEAD", toFlags appDir [])
          (('-':_):_) -> ("HEAD", toFlags appDir args)
          (r:rest) -> (r, toFlags appDir rest)
  fs <- readProcess "git" ["ls-tree", "-r", "-t", "--name-only", rev] ""
  unchangedFiles <- checkUnchanged (flags Map.! "-f") $ lines fs
  if null unchangedFiles
    then report "No unchanged files was found"
    else do
    report "Restoring modification time for all these files:"
    mapM_ (restoreFileModtime rev) unchangedFiles
#endif
