module Voting where

import Data.List qualified as List
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector

-- | Computes the disutility associated with a voter and a possible candidate.
-- In other words, how unhappy the voter would be with the choice of a
-- candidate, based on their disagreement about each axis of political affinity.
-- In this spatial mode, this is the distance between two vectors.
disutility :: Vector Double -> Vector Double -> Double
disutility p1 p2 =
  sqrt $ Vector.sum $ Vector.zipWith (\x y -> (x - y) * (x - y)) p1 p2

disutilities :: [Vector Double] -> Vector Double -> Map Int Double
disutilities candidates voter =
  Map.fromList [(i, disutility c voter) | (i, c) <- zip [1 ..] candidates]

-- | In a ranked ballot election, the percent of the voting population who cast
-- a ballot with each candidate ordering.
type RankMap a = Map [Int] a

-- | The ranked ballot that is cast by this voter based on their distance to
-- each candidate in the space of political affinities.
rankedBallot :: [Vector Double] -> Vector Double -> [Int]
rankedBallot candidates voter =
  fmap fst . List.sortOn snd . Map.toList $ disutilities candidates voter

-- | The collective ranked ballots cast by a population of voters, given a list
-- of candidates.
rankedBallots :: [Vector Double] -> [Vector Double] -> RankMap Double
rankedBallots voters candidates =
  fmap (/ fromIntegral (length voters))
    . Map.fromListWith (+)
    . map ((,1) . rankedBallot candidates)
    $ voters

-- | The result of adjusting candidate ballots.  This is a step in several
-- voting processes, for instance when a candidate is eliminated.
mapRanks :: Num a => ([Int] -> [Int]) -> RankMap a -> RankMap a
mapRanks f = Map.delete [] . Map.mapKeysWith (+) f

-- Given a score for each candidate, compute the ranking of candidates by
-- decreasing score.
rankByRating :: Map Int Double -> [Int]
rankByRating ballots =
  fst <$> List.sortOn (Down . snd) (Map.toList ballots)

-- | Given each voter's rating ballot, compute the collective rating ballot by
-- adding each voter's scores.
collectRangeBallots :: [Map Int Double] -> Map Int Double
collectRangeBallots = Map.unionsWith (+)
