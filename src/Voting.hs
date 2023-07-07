module Voting where

import Data.Function (on)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector

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

-- | The result of adjusting candidate ballots.  This is a step in several
-- voting processes, for instance when a candidate is eliminated.
mapRanks :: Num a => ([Int] -> [Int]) -> RankMap a -> RankMap a
mapRanks f = Map.delete [] . Map.mapKeysWith (+) f

-- | A ranking of candidates by utility.  This isn't a realistic voting model,
-- but it's a useful tool for comparing voting systems to see how well they
-- reflect the actual preferences of voters.
utilityRanking :: [Vector Double] -> [Vector Double] -> [[Int]]
utilityRanking voters candidates =
  fmap (fmap fst) $
    List.groupBy ((==) `on` snd) $
      List.sortOn
        snd
        [ (i, sum (disutility c <$> voters))
          | (i, c) <- zip [1 ..] candidates
        ]
