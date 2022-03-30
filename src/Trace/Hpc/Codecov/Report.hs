{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
-- |
-- Module:     Trace.Hpc.Codecov.Report
-- Copyright:  (c) 2022 8c6794b6
-- License:    BSD3
-- Maintainer: 8c6794b6 <8c6794b6@gmail.com>
--
-- Generate Codecov report data.

module Trace.Hpc.Codecov.Report
  ( -- * Types
    Report(..)
  , CoverageEntry(..)
  , LineHits
  , Hit(..)

    -- * Functions
  , genReport
  , genCoverageEntries
  ) where

-- base
import Control.Exception           (ErrorCall, handle, throw, throwIO)
import Control.Monad               (mplus, when)
import Data.Function               (on)
import System.IO                   (hPutStrLn, stderr)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid                 ((<>))
#endif

-- directory
import System.Directory            (doesFileExist)

-- filepath
import System.FilePath             ((<.>), (</>))

-- hpc
import Trace.Hpc.Mix               (BoxLabel (..), Mix (..), readMix)
import Trace.Hpc.Tix               (Tix (..), TixModule (..), readTix)
import Trace.Hpc.Util              (HpcPos, fromHpcPos)

-- Internal
import Trace.Hpc.Codecov.Exception

import qualified Text.XML.Light as XML
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import Data.List (partition, foldl')
import Data.Bifunctor (Bifunctor (bimap))
-- ------------------------------------------------------------------------
--
-- Exported
--
-- ------------------------------------------------------------------------

-- | Data type to hold information for generating test coverage
-- report.
data Report = Report
 { reportTix      :: FilePath
   -- ^ Input tix file.
 , reportMixDirs  :: [FilePath]
   -- ^ Directories containing mix files referred by the tix file.
 , reportSrcDirs  :: [FilePath]
   -- ^ Directories containing source codes referred by the mix files.
 , reportExcludes :: [String]
   -- ^ Module name strings to exclude from coverage report.
 , reportOutFile  :: Maybe FilePath
   -- ^ Output file to write JSON report, if given.
 , reportVerbose  :: Bool
   -- ^ Flag for showing verbose message during report generation.
 } deriving (Eq, Show)

#if MIN_VERSION_base(4,11,0)
instance Semigroup Report where
  (<>) = mappendReport
#endif

instance Monoid Report where
  mempty = emptyReport
#if !MIN_VERSION_base(4,16,0)
  mappend = mappendReport
#endif

emptyReport :: Report
emptyReport = Report
  { reportTix = throw NoTarget
  , reportMixDirs = []
  , reportSrcDirs = []
  , reportExcludes = []
  , reportOutFile = Nothing
  , reportVerbose = False
  }

mappendReport :: Report -> Report -> Report
mappendReport r1 r2 =
  let extend f = ((<>) `on` f) r1 r2
  in  Report { reportTix = reportTix r2
             , reportMixDirs = extend reportMixDirs
             , reportSrcDirs = extend reportSrcDirs
             , reportExcludes = extend reportExcludes
             , reportOutFile = (mplus `on` reportOutFile) r1 r2
             , reportVerbose = ((||) `on` reportVerbose) r1 r2
             }

-- | Single file entry in coverage report.
--
-- See the
-- <https://docs.codecov.io/docs/codecov-custom-coverage-format Codecov documentation>
-- for detail.
data CoverageEntry =
  CoverageEntry { ce_filename :: FilePath -- ^ Source code file name.
                , ce_hits     :: [(LineNum, CovInfo)]
                } deriving (Eq, Show)

-- | Pair of line number and hit tag.
type LineHits = [(Int, Hit)]

-- | Data type to represent coverage of source code line.
data Hit
  = Missed  -- ^ The line is not covered at all.
  | Partial -- ^ The line is partially covered.
  | Full    -- ^ The line is fully covered.
  deriving (Eq, Show)

-- | Generate report data from options.
genReport :: Report -> IO ()
genReport rpt =
  do entries <- genCoverageEntries rpt
     let mb_out = reportOutFile rpt
         oname = maybe "stdout" show mb_out
     say rpt ("Writing JSON report to " ++ oname)
     -- emitCoverageJSON mb_out entries -- FIXME
     emitCoverageXML mb_out entries
     say rpt "Done"

-- | Generate test coverage entries.
genCoverageEntries :: Report -> IO [CoverageEntry]
genCoverageEntries rpt =
  readTixFile rpt (reportTix rpt) >>= tixToCoverage rpt

emitCoverageXML :: Maybe FilePath -> [CoverageEntry] -> IO ()
emitCoverageXML fp'm es = case fp'm of
  Just file -> writeFile file $ buildXML es
  Nothing -> putStrLn $ buildXML es



-- ------------------------------------------------------------------------
--
-- Internal
--
-- ------------------------------------------------------------------------
buildXML :: [CoverageEntry] -> String
buildXML es = XML.ppcElement XML.prettyConfigPP el
  where
    el :: XML.Element
    el = (XML.node (qn "coverage") (XML.Attr (qn "version") "1"))
         { XML.elContent = map (XML.Elem . toFileElem) es }

    toFileElem :: CoverageEntry -> XML.Element
    toFileElem (CoverageEntry file hits) =
      (XML.node (qn "file") (XML.Attr (qn "path") file))
      { XML.elContent = map (XML.Elem . toLineElem) hits }

    toLineElem :: (Int, CovInfo) -> XML.Element
    toLineElem (lineNumber, covInfo) =
      XML.node (qn "lineToCover") $
       [XML.Attr (qn "lineNumber") (show lineNumber)] <> covInfoToCoveredAttrs covInfo

    covInfoToCoveredAttrs :: CovInfo -> [XML.Attr]
    covInfoToCoveredAttrs CiCovered = [mkAttr "covered" "true"]
    covInfoToCoveredAttrs CiNotCovered = [mkAttr "covered" "false"]
    covInfoToCoveredAttrs CiPartial{covCnt, notCovCnt} =
      [ mkAttr "covered" "true"
      , mkAttr "branchesToCover" (show $ covCnt + notCovCnt)
      , mkAttr "coveredBranches" (show covCnt)
      ]
    mkAttr :: String -> String -> XML.Attr
    mkAttr n v = XML.Attr (qn n) v


qn :: String -> XML.QName
qn n = XML.QName n Nothing Nothing

tixToCoverage :: Report -> Tix -> IO [CoverageEntry]
tixToCoverage rpt (Tix tms) = mapM (tixModuleToCoverage rpt)
                                   (excludeModules rpt tms)

tixModuleToCoverage :: Report -> TixModule -> IO CoverageEntry
tixModuleToCoverage rpt tm@(TixModule name _hash _count ixs) =
  do say rpt ("Search mix:   " ++ name)
     Mix path _ _ _ entries <- readMixFile (reportMixDirs rpt) tm
     say rpt ("Found mix:    "++ path)

     let lineHits = makeInfo ixs entries
     path' <- ensureSrcPath rpt path
     return (CoverageEntry { ce_filename = path'
                           , ce_hits = lineHits })

data CovInfo = CiCovered
             | CiNotCovered
             | CiPartial { covCnt :: !Int
                         , notCovCnt :: ! Int
                         } deriving (Eq, Show)

makeInfo :: [Integer] -> [(HpcPos, BoxLabel)] -> [(LineNum, CovInfo)]
makeInfo ticks mes =
  let infos = uncurry zip3 (unzip mes) $ map (>0) ticks
  in M.toAscList $
     M.map toCovInfo $
     foldl' f M.empty infos
  where
    toCovInfo :: [Bool] -> CovInfo
    toCovInfo xs = case bimap length length $ partition (== True) xs of
      (0, _) -> CiNotCovered
      (_, 0) -> CiCovered
      (covCnt, notCovCnt) -> CiPartial {covCnt, notCovCnt}

    f :: Map LineNum [Bool] -> (HpcPos, BoxLabel, Bool) -> Map LineNum [Bool]
    f acc (hpcPos, lab, cov) = case lab of
      ExpBox _ -> markSpan acc [ls .. le] cov
      TopLevelBox _ -> acc
      LocalBox _ -> markSpan acc [ls .. le] cov
      BinBox _ _ -> acc
      where
        (ls, _, le, _) = fromHpcPos hpcPos

    markSpan :: Map LineNum [Bool] -> [LineNum] -> Bool -> Map LineNum [Bool]
    markSpan m ls cov = foldl' g m ls
      where
        g :: Map LineNum [Bool] -> LineNum -> Map LineNum [Bool]
        g m' l = M.alter h l m'

        h :: Maybe [Bool] -> Maybe [Bool]
        h Nothing = Just [cov]
        h (Just vs) = Just $ cov:vs

type LineNum = Int

-- | Exclude modules specified in given 'Report'.
excludeModules :: Report -> [TixModule] -> [TixModule]
excludeModules rpt = filter exclude
  where
    exclude (TixModule pkg_slash_name _ _ _) =
      let modname = case break (== '/') pkg_slash_name of
                      (_, '/':name) -> name
                      (name, _)     -> name
      in  notElem modname (reportExcludes rpt)

-- | Read tix file from file path, return a 'Tix' data or throw
-- a 'TixNotFound' exception.
readTixFile :: Report -> FilePath -> IO Tix
readTixFile rpt path =
  do mb_tix <- readTix path
     case mb_tix of
       Nothing  -> throwIO (TixNotFound path)
       Just tix -> say rpt ("Found tix file: " ++ path) >> return tix

-- | Search mix file under given directories, return a 'Mix' data or
-- throw a 'MixNotFound' exception.
readMixFile :: [FilePath] -> TixModule -> IO Mix
readMixFile dirs tm@(TixModule name _h _c _i) =
  handle handler (readMix dirs (Right tm))
  where
    handler :: ErrorCall -> IO a
    handler _ = throwIO (MixNotFound name dirs')
    dirs' = map (</> (name <.> "mix")) dirs

-- | Ensure the given source file exist, return the ensured 'FilePath'
-- or throw a 'SrcNotFound' exception.
ensureSrcPath :: Report -> FilePath -> IO FilePath
ensureSrcPath rpt path = go [] (reportSrcDirs rpt)
  where
    go acc [] = throwIO (SrcNotFound path acc)
    go acc (dir:dirs) =
      do let path' = dir </> path
         exist <- doesFileExist path'
         if exist
            then do say rpt ("Found source: " ++ path')
                    return path'
            else go (path':acc) dirs

-- | Print given message to 'stderr' when the verbose flag is 'True'.
say :: Report -> String -> IO ()
say rpt msg = when (reportVerbose rpt) (hPutStrLn stderr msg)
