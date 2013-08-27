{-# LANGUAGE TemplateHaskell #-}

module Fasta where

import Control.Lens ((^.), (.~), (%~), _1, _2, both, makeLenses, over)
import Control.Monad.State (State, execState, modify, when)
import Data.List (sort)
import Data.Maybe (fromMaybe)

import qualified Data.Map as Map
import qualified Data.Traversable as Trav

k              = 1
scoreThreshold = 0
gapThreshold   = 4
hitScore       = 5
missScore      = -4

gapStartPenalty     = -5
gapExtensionPenalty = -100

gapDeltas :: [Int]
gapDeltas = take gapThreshold [-gapThreshold..] ++
            take gapThreshold [1..]

type IndexMap = Map.Map String [Int]
type Dotplot = Map.Map String [(Int, Int)]

data DiagonalInfo = DiagonalInfo { _dIndex :: Int
                                 , _dScore :: Int
                                 , _dStart :: (Int, Int)
                                 , _dStop  :: (Int, Int)
                                 } deriving (Show)

makeLenses ''DiagonalInfo

instance Eq DiagonalInfo where
   d1 == d2 = d1 ^. dScore == d2 ^. dScore

instance Ord DiagonalInfo where
   d1 <= d2 = d1 ^. dScore <= d2 ^. dScore

type DiagonalScores = Map.Map Int DiagonalInfo

data Region = SingleDiagonal DiagonalInfo
            | DoubleDiagonal DiagonalInfo DiagonalInfo
            deriving (Show)

instance Eq Region where
   r1 == r2 = regionScore r1 == regionScore r2

instance Ord Region where
   r1 <= r2 = regionScore r1 <= regionScore r2

type RegionScores = Map.Map Int Region

regionScore :: Region -> Int
regionScore (SingleDiagonal diag_info) = diag_info ^. dScore
regionScore (DoubleDiagonal diag_info1 diag_info2) =
   diag_info1 ^. dScore + diag_info2 ^. dScore + gapPenalty gap_size
   where gap_size = gapSize diag_info1 diag_info2

gapPenalty :: Int -> Int
gapPenalty gap_size = gapStartPenalty + gapExtensionPenalty * (gap_size - 1)

gapSize :: DiagonalInfo -> DiagonalInfo -> Int
gapSize diag_info1 diag_info2 = abs $ diag_info1 ^. dIndex - diag_info2 ^. dIndex

initIndexMap :: IndexMap
initIndexMap = Map.empty

adjustWithDefault :: Ord k => a -> (a -> a) -> k -> Map.Map k a -> Map.Map k a
adjustWithDefault def f key m =
   if key `Map.member` m
   then Map.adjust f key m
   else Map.insert key def m

indexMap :: String -> IndexMap
indexMap s = execState (indexMap' 0 s) initIndexMap
   where indexMap' :: Int -> String -> State IndexMap ()
         indexMap' index (c:cs) = do
            let key = take k (c:cs)
            when (length key == k) $ do
               modify $ adjustWithDefault [index] (index:) key
               indexMap' (index + 1) cs
         indexMap' _ [] = return ()

dotplot :: String -> String -> Dotplot
dotplot s1 s2 =
   let
      im1 = indexMap s1
      im2 = indexMap s2
   in
      Map.filter (not . null) $
         Map.mapWithKey (\key val -> do
            x <- val
            y <- fromMaybe [] $ Map.lookup key im2
            return (x,y)
         ) im1

diagIndex :: Int -> (Int, Int) -> Int
diagIndex n (i, j) = j - i + (n - 1)

diagonalScores :: Int -> Dotplot -> DiagonalScores
diagonalScores n dp = execState (diagonalScores' dp) Map.empty
   where diagonalScores' :: Dotplot -> State DiagonalScores ()
         diagonalScores' dp' = do
            _ <- Trav.mapM toDiagScore dp'
            modify $ takeN 10 . Map.filter ((>= scoreThreshold) . (^. dScore))

         takeN :: (Ord k, Ord v) => Int -> Map.Map k v -> Map.Map k v
         takeN n' = Map.fromList . take n' . sort . Map.toList

         toDiagScore :: [(Int, Int)] -> State DiagonalScores ()
         toDiagScore =
            mapM_ (\x -> do
               let (s1_index, s2_index) = x

               let adjustStart1 = (dStart . _1) %~ min s1_index
               let adjustStart2 = (dStart . _2) %~ min s2_index

               let adjustStop1 = (dStop . _1) %~ max (s1_index + k - 1)
               let adjustStop2 = (dStop . _2) %~ max (s2_index + k - 1)

               let adjustScore = dScore %~ (+1)
               let adjustStart = adjustStart1 . adjustStart2
               let adjustStop  = adjustStop1  . adjustStop2

               let diag_index = diagIndex n x

               modify $
                  adjustWithDefault
                     (DiagonalInfo diag_index 1 x (over both (+(k-1)) x)) -- if insert
                     (adjustScore . adjustStart . adjustStop)             -- if adjust
                     diag_index                                           -- key
                  )

scoreStrings :: String -> String -> Int
scoreStrings []     []     = 0
scoreStrings (x:xs) (y:ys) = scoreChars x y + scoreStrings xs ys
   where scoreChars :: Char -> Char -> Int
         scoreChars c1 c2 = if c1 == c2 then hitScore else missScore
scoreStrings _      _      = error "scoreStrings expects strings of the same length"

diagInfoSubstrings :: String -> String -> DiagonalInfo -> (String, String)
diagInfoSubstrings s1 s2 diag_info =
   let drop_s1 = drop $ diag_info ^. (dStart . _1)
       drop_s2 = drop $ diag_info ^. (dStart . _2)
       take_s1 = take $ diag_info ^. (dStop . _1) - diag_info ^. (dStart . _1) + 1
       take_s2 = take $ diag_info ^. (dStop . _2) - diag_info ^. (dStart . _2) + 1
   in ((take_s1 . drop_s1) s1, (take_s2 . drop_s2) s2)

rescoreDiagonals :: String -> String -> DiagonalScores -> DiagonalScores
rescoreDiagonals s1 s2 = Map.map (\diag_index ->
   let (s1', s2') = diagInfoSubstrings s1 s2 diag_index
   in dScore .~ scoreStrings s1' s2' $ diag_index)

calculateRegions :: DiagonalScores -> RegionScores
calculateRegions ds = Map.map (diagonalInfoToRegion ds) ds

diagonalInfoToRegion :: DiagonalScores -> DiagonalInfo -> Region
diagonalInfoToRegion ds diag_info =
   let
      diag_indices = map (+ diag_info ^. dIndex) gapDeltas
      ds' = Map.filterWithKey (\key _ -> key `elem` diag_indices) ds
   in
      Map.fold (\diag_info' region ->
         max region $ DoubleDiagonal diag_info diag_info'
      ) (SingleDiagonal diag_info) ds'

_s1 = "ABCDE"
_s2 = "ABDEFCDE"
_dp = dotplot _s1 _s2
_ds = diagonalScores (length _s1) _dp
_ds' = rescoreDiagonals _s1 _s2 _ds
