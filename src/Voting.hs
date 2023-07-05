module Voting where

import Data.List qualified as List
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Vector.Unboxed (Vector)
import Model (distance)

-- | In a ranked ballot election, the percent of the voting population who cast
-- a ballot with each candidate ordering.
type RankedBallots = Map [Int] Double

-- | The ranked ballot that is cast by this voter based on their distance to
-- each candidate in the space of political affinities.
rankedBallot :: [Vector Double] -> Vector Double -> [Int]
rankedBallot candidates voter =
  map snd $ List.sort $ zip (distance voter <$> candidates) [1 ..]

-- | The collective ranked ballots cast by a population of voters, given a list
-- of candidates.
rankedBallots :: [Vector Double] -> [Vector Double] -> RankedBallots
rankedBallots voters candidates =
  fmap (/ fromIntegral (length voters))
    . Map.fromListWith (+)
    . map ((,1) . rankedBallot candidates)
    $ voters

-- | The result of adjusting candidate ballots.  This is a step in several
-- voting processes, for instance when a candidate is eliminated.
mapBallots :: ([Int] -> [Int]) -> RankedBallots -> RankedBallots
mapBallots f = Map.delete [] . Map.mapKeysWith (+) f

-- Given a score for each candidate, compute the ranking of candidates by
-- decreasing score.
rankByRating :: Map Int Double -> [Int]
rankByRating ballots =
  fst <$> List.sortOn (Down . snd) (Map.toList ballots)

-- | Given each voter's rating ballot, compute the collective rating ballot by
-- adding each voter's scores.
collectRangeBallots :: [Map Int Double] -> Map Int Double
collectRangeBallots = Map.unionsWith (+)
