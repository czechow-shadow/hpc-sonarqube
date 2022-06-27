{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE ViewPatterns     #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
-- |
-- Module:     Trace.Hpc.SonarQube.Report
-- Copyright:  (c) 2022 8c6794b6
-- License:    BSD3
-- Maintainer: 8c6794b6 <8c6794b6@gmail.com>
--
-- Generate SonarQube report data.
--
-- Adapted from Codecov by SCB TSTRATS team.

module Trace.Hpc.SonarQube.Report
  ( -- * Types
    Report(..)
    -- * Functions
  , genReport
  ) where

-- base
import Control.Exception           (ErrorCall, handle, throw, throwIO)
import Control.Monad               (mplus, when)
import Data.Function               (on)
import System.IO                   (hPutStrLn, stderr)

-- directory
import System.Directory            (doesFileExist)

-- filepath
import System.FilePath             ((<.>), (</>))

-- hpc
import Trace.Hpc.Mix               (BoxLabel (..), Mix (..), readMix, MixEntry)
import Trace.Hpc.Tix               (Tix (..), TixModule (..), readTix)
import Trace.Hpc.Util              (fromHpcPos)

-- Internal
import Trace.Hpc.SonarQube.Exception

import qualified Text.XML.Light as XML
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import Data.List (partition, sort)
import Data.Bifunctor (Bifunctor (bimap))
import Control.Monad.State (State, modify)
import Control.Monad.State.Lazy (runState)
import Control.Arrow (second)
import Data.Maybe (catMaybes)
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
   -- ^ Output file to write XML report, if given.
 , reportVerbose  :: Bool
   -- ^ Flag for showing verbose message during report generation.
 } deriving (Eq, Show)

instance Semigroup Report where
  (<>) = mappendReport

instance Monoid Report where
  mempty = emptyReport
  mappend = mappendReport

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
-- <https://docs.sonarqube.org/8.9/analysis/generic-test/ SonarQube documentation>
-- for detail.
data CoverageEntry =
  CoverageEntry { ce_filename :: FilePath -- ^ Source code file name.
                , ce_hits     :: [(LineNum, CovInfo)]
                } deriving (Eq, Show)

-----------------------------------------------------------------------------
data Marker = Beg | End deriving (Show, Eq, Ord)
type HitCnt = Integer
type LineNum = Int
type ColNum = Int

data MixEntryCoverage = MeCovered
                      | MeNotCovered
                      deriving (Show, Eq, Ord)

data LineMarker = LineMarker { col    :: !ColNum
                             , marker :: !Marker
                             , cov    :: !MixEntryCoverage
                             } deriving (Show, Eq, Ord)

data LineExpsStat = LineExpsStat { opened :: !Int
                                 , full   :: !Int -- both opened and closed
                                 , closed :: !Int
                                 } deriving (Show, Eq, Ord)

emptyLineExpsStat :: LineExpsStat
emptyLineExpsStat = LineExpsStat 0 0 0

data CovInfo = CiCovered    { stat :: !LineExpsStat }
             | CiNotCovered { stat :: !LineExpsStat }
             | CiPartial { covCnt     :: !Int
                         , notCovCnt  :: !Int
                         , covStat    :: !LineExpsStat
                         , notCovStat :: !LineExpsStat
                         } deriving (Eq, Show)
-----------------------------------------------------------------------------

-- | Generate report data from options.
genReport :: Report -> IO ()
genReport rpt =
  do entries <- genCoverageEntries rpt
     let mb_out = reportOutFile rpt
         oname = maybe "stdout" show mb_out
     say rpt ("Writing XML report to " ++ oname)
     emitCoverageXML mb_out entries
     say rpt "Done"

-- | Generate test coverage entries.
genCoverageEntries :: Report -> IO [CoverageEntry]
genCoverageEntries rpt =
  readTixFile rpt (reportTix rpt) >>= tixToCoverage rpt

emitCoverageXML :: Maybe FilePath -> [CoverageEntry] -> IO ()
emitCoverageXML fp'm es = case fp'm of
  Just file -> writeFile file $ buildXML es
  Nothing   -> putStrLn $ buildXML es

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
    covInfoToCoveredAttrs CiCovered{} = [mkAttr "covered" "true"]
    covInfoToCoveredAttrs CiNotCovered{} = [mkAttr "covered" "false"]
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

calcMarkers :: [(MixEntry, HitCnt)] -> [(LineNum, [LineMarker])]
calcMarkers = M.toAscList . fmap sort . foldl f M.empty . sort
  where
    f :: Map LineNum [LineMarker] -> (MixEntry, HitCnt) -> Map LineNum [LineMarker]
    f m (mixEntry, cov) =
      let lineInfos = toLineMarkers mixEntry $ toMixEntryCoverage cov
          ms = m : map (uncurry M.singleton . second pure) lineInfos
          m' = sort <$> M.unionsWith (++) ms
      in fillEmptyLines m'

    fillEmptyLines :: Monoid a => Map LineNum a -> Map LineNum a
    fillEmptyLines m = case (,) <$> M.lookupMin m <*> M.lookupMax m of
      Just ((mnl, _), (mxl, _)) ->
        M.unionWith (<>) m $ M.fromAscList $ zip [mnl .. mxl] $ repeat mempty
      Nothing -> m

    toMixEntryCoverage :: HitCnt  -> MixEntryCoverage
    toMixEntryCoverage 0 = MeNotCovered
    toMixEntryCoverage _ = MeCovered

calcExps :: [LineMarker] -> LineExpsStat
calcExps = foldl f emptyLineExpsStat
  where
    f :: LineExpsStat -> LineMarker -> LineExpsStat
    f LineExpsStat{opened, ..} LineMarker{marker = Beg} =
      LineExpsStat {opened = succ opened, ..}
    f LineExpsStat{opened, full, closed} LineMarker{marker = End}
      | opened > 0 = LineExpsStat {opened = pred opened, full = succ full, ..}
      | otherwise = LineExpsStat {closed = succ closed, ..}

-- We are going to need state information when dealing with some
-- degenerate cases...
calcCov :: [(LineNum, [LineMarker])] -> [(LineNum, CovInfo)]
calcCov xs = case flip runState [] $ mapM f xs of
  (res, st) -> case st of
    [] -> catMaybes res
    _  -> error $ "Exp stack is not clean (malformed coverage info): " <> show st
  where
    f :: (LineNum, [LineMarker]) -> State [LineMarker] (Maybe (LineNum, CovInfo))
    f (lno, markers) =  do
      modify $ \st -> applyToStack st markers
      let (covStat, notCovStat) = bimap calcExps calcExps $
                                  partition ((MeCovered ==) . cov) markers
      pure $ fmap (lno,) $
        case (countExps covStat, countExps notCovStat) of
          (0, 0) -> Nothing
          (_, 0) -> Just $ CiCovered covStat
          (0, _) -> Just $ CiNotCovered notCovStat
          (covCnt, notCovCnt) ->
            Just CiPartial {covCnt, notCovCnt, covStat, notCovStat}

   -- Rules for counting expression in line:
    -- - expression starts in a line
    -- - whole expression fits in a line
    -- - ignore (do not count) expression ends
    countExps :: LineExpsStat -> Int
    countExps LineExpsStat{opened, full} = opened + full

applyToStack :: [LineMarker] -> [LineMarker] -> [LineMarker]
applyToStack = foldl f
  where
    f :: [LineMarker] -> LineMarker -> [LineMarker]
    f [] i@LineMarker{marker = End} =
      error $ "Trying to pop " <> show i <> " from empty exp stack"
    f (_:st') LineMarker{marker = End} = st'
    f st' i@LineMarker{marker = Beg} = i:st'

-- ExpBox contains all coverage information we need,
-- and it is ok to ignore all other data constructors:
--   TopLevelBox _ -> [] -- top level fun/term definition
--   LocalBox _    -> [] -- local (in the "where" clause) fun/term definition
--   BinBox _ _    -> [] -- Alternative ("if", "guard" etc)
-- See also: the "Haskell Program Coverage" paper
-- (available here http://ittc.ku.edu/~andygill/papers/Hpc07.pdf)
toLineMarkers :: MixEntry -> MixEntryCoverage -> [(LineNum, LineMarker)]
toLineMarkers (fromHpcPos -> (l1, c1, l2, c2), ExpBox _) cov =
    [ (l1, LineMarker {col = c1, marker = Beg, cov})
    , (l2, LineMarker {col = c2, marker = End, cov})
    ]
toLineMarkers _ _ = []

tixModuleToCoverage :: Report -> TixModule -> IO CoverageEntry
tixModuleToCoverage rpt tm@(TixModule name _hash _count ixs) =
  do say rpt "<!--"
     say rpt ("Search mix:   " ++ name)
     Mix path _ _ _ entries <- readMixFile (reportMixDirs rpt) tm
     say rpt ("Found mix:    "++ path)

     say rpt $ "===> Beg bare " <> path
     mapM_ (say rpt . show) $ sort $ zip entries ixs
     say rpt "<=== End bare"

     let lineHits = calcCov $ calcMarkers $ zip entries ixs

     say rpt $ "===> Beg cov " <> path
     mapM_ (say rpt . show) lineHits
     say rpt "<=== End cov"

     path' <- ensureSrcPath rpt path
     say rpt "-->"
     return CoverageEntry {ce_filename = path', ce_hits = lineHits}

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
