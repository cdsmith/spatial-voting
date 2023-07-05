module Elections where

import Data.List qualified as List
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Vector.Unboxed (Vector)
import Model (distance)
import Voting (RankedBallots, collectRangeBallots, mapBallots, rankByRating)

-- | A ranking of candidates by utility.  This isn't a realistic voting model,
-- but it's a useful tool for comparing voting systems to see how well they
-- reflect the actual preferences of voters.
utilityRanking :: [Vector Double] -> [Vector Double] -> [Int]
utilityRanking voters candidates =
  fst
    <$> List.sortOn
      snd
      [ (i, sum (distance c <$> voters))
        | (i, c) <- zip [1 ..] candidates
      ]

-- | A ballot that would be cast by a voter in a range voting election, if they
-- are honest about their preferences.  They rank their least preferred
-- candidate zero, their most preferred candidate one, and interpolate linearly
-- in between.
honestRangeBallot :: [Vector Double] -> Vector Double -> Map Int Double
honestRangeBallot candidates voter =
  Map.fromList [(c, (d1 - d) / (d1 - d0)) | (c, d) <- zip [1 ..] distances]
  where
    distances = distance voter <$> candidates
    d0 = minimum distances
    d1 = maximum distances

-- | A ballot that would be cast by a voter in a range voting election, if they
-- are purely tactical about their preferences.  They consider the candidates
-- most likely to win, and cast their ballot to maximize their influence on
-- those candidates by assigning them minimum and maximum scores.  Secondary
-- preferences that lie between the two can still receive intermediate scores.
tacticalRangeBallot :: [Int] -> [Int] -> Map Int Double
tacticalRangeBallot likelyWinners preferences =
  Map.fromList (go likelyWinners 0)
  where
    go (c1 : c2 : candidates) squeeze =
      let (good, bad)
            | c1 `preferredTo` c2 = (c1, c2)
            | otherwise = (c2, c1)
          (better, candidates') = List.partition (`preferredTo` good) candidates
          (worse, candidates'') = List.partition (bad `preferredTo`) candidates'
       in ((,1 - squeeze) <$> c1 : better)
            ++ ((,squeeze) <$> c2 : worse)
            ++ go candidates'' ((squeeze + 1) / 3)
    go [c] _ = [(c, 0.5)]
    go [] _ = []

    preferredTo c1 c2 =
      List.elemIndex c1 preferences < List.elemIndex c2 preferences

-- | A ballot that would be cast by a voter in a range voting election, if they
-- are tactical about their preferences, but only to a limited extent.  They
-- exaggerate their preferences in a tactical way by taking a weighted average
-- of their honest and tactical ballots.
mixedRangeBallot ::
  Double -> [Int] -> [Vector Double] -> Vector Double -> Map Int Double
mixedRangeBallot p likelyWinners candidates voter =
  Map.intersectionWith
    (\a b -> p * a + (1 - p) * b)
    (tacticalRangeBallot likelyWinners preferences)
    (honestRangeBallot candidates voter)
  where
    preferences = utilityRanking [voter] candidates

-- | Given a list of voters and a list of candidates, compute the collective
-- range election result.  The first argument indicates whether the voter is
-- tactical, and if so, how much they exaggerate their preferences and which
-- candidates they consider likely to win.
rangeRating ::
  Maybe (Double, [Int]) -> [Vector Double] -> [Vector Double] -> Map Int Double
rangeRating tactics voters candidates =
  collectRangeBallots (ballot <$> voters)
  where
    ballot = case tactics of
      Nothing -> honestRangeBallot candidates
      Just (p, likelyWinners) -> mixedRangeBallot p likelyWinners candidates

-- | Given ranked ballot results, compute the plurality ranking of candidates.
-- This is the ranking by number of first place votes, also known (though
-- misleadingly) as "first past the post".
pluralityRanking :: RankedBallots -> [Int]
pluralityRanking ballots = ranked ++ List.sort unranked
  where
    candidates = head $ Map.keys ballots
    ranked =
      fst
        <$> List.sortOn
          (Down . snd)
          (Map.toList (Map.mapKeysWith (+) head ballots))
    unranked = candidates List.\\ ranked

-- | A tactical modification made by a voter to their ranked ballot to optimize
-- their influence on the outcome of a plurality election.  They will choose
-- their favorite out of the two candidates they consider most likely to win,
-- and rank that candidate first, and the rest afterward.
pluralityTactics :: [Int] -> [Int] -> [Int]
pluralityTactics likelyWinners preferences =
  champion : List.delete champion preferences
  where
    likely = take 2 likelyWinners
    champion = head (filter (`elem` likely) preferences)

-- | Given ranked ballot results, compute the generalized IRV ranking of
-- candidates.  This generalization allows you to substitute any elimination
-- rule to decide which candidate to eliminate at each step.
generalizedIrvRanking :: (RankedBallots -> Int) -> RankedBallots -> [Int]
generalizedIrvRanking elimRule ballots
  | length ballots == 1 = head (Map.keys ballots)
  | otherwise =
      generalizedIrvRanking
        elimRule
        (mapBallots (List.delete worst) ballots)
        ++ [worst]
  where
    worst = elimRule ballots

-- | Given ranked ballot results, compute the IRV ranking of candidates.  The
-- ranking is based on the order candidates are eliminated.
irvRanking :: RankedBallots -> [Int]
irvRanking = generalizedIrvRanking (last . pluralityRanking)

-- | Dualizes an election rule, by using it to choose the worst candidates
-- by reversing the rankings on each ballot, then reversing the result.
--
-- Some election rules, such as Smith/Condorcet, Borda count, and range voting,
-- are unaffected by this transformation.  Others do something very different.
dual :: (RankedBallots -> [a]) -> RankedBallots -> [a]
dual f ballots = reverse (f (Map.mapKeysWith (+) reverse ballots))

-- | Given ranked ballot results, compute the head-to-head result of a pair of
-- candidates.  This is the percent of voters who prefer the first candidate to
-- the second.
headToHead :: RankedBallots -> Int -> Int -> Double
headToHead ballots c c' =
  sum
    [ ballots Map.! r
      | r <- Map.keys ballots,
        List.elemIndex c r < List.elemIndex c' r
    ]

-- | Given ranked ballot results, compute the head-to-head matrix of all pairs
-- of candidates.  This is the percent of voters who prefer each candidate to
-- each other candidate.
h2hMatrix :: RankedBallots -> [[Double]]
h2hMatrix ballots = [[headToHead ballots c c' | c' <- [1 .. n]] | c <- [1 .. n]]
  where
    n = maximum (head $ Map.keys ballots) + 1

-- | Given ranked ballot results, compute the Condorcet winner, if one exists.
condorcetWinner :: RankedBallots -> Maybe Int
condorcetWinner ballots =
  listToMaybe
    [ c
      | c <- candidates,
        all
          (\c' -> h2h !! (c - 1) !! (c' - 1) > 0.5)
          (List.delete c candidates)
    ]
  where
    candidates = head $ Map.keys ballots
    h2h = h2hMatrix ballots

-- | Compute all subsequences of a given length.
subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize 0 _ = [[]]
subsequencesOfSize _ [] = []
subsequencesOfSize n (x : xs) =
  ((x :) <$> subsequencesOfSize (n - 1) xs) ++ subsequencesOfSize n xs

-- | Given ranked ballot results, compute the Smith set.  This is the smallest
-- non-empty set of candidates who are all preferred to any candidate not in the
-- set.  If there is a Condorcet winner, the Smith set is just that candidate;
-- otherwise, it contains three or more candidates who are essentially tied by
-- the Condorcet criterion.
smithSet :: RankedBallots -> [Int]
smithSet ballots = head (filter allBeatenBySet options)
  where
    candidates = head (Map.keys ballots)
    h2h = h2hMatrix ballots
    allBeatenBySet set =
      flip all set $ \c ->
        flip all (candidates List.\\ set) $ \c' ->
          h2h !! (c - 1) !! (c' - 1) > 0.5
    options =
      concatMap
        (`subsequencesOfSize` candidates)
        [1 .. length candidates]

-- | Given ranked ballot results, compute the Smith ranking.  This is the
-- partition of candidates into Smith sets, where each set contains candidates
-- who would lose to any candidate in an earlier set, but beat any candidate in
-- a later set.  Candidates within each set are essentially tied by the
-- Condorcet criterion.
smithRanking :: RankedBallots -> [[Int]]
smithRanking ballots
  | Map.null ballots = []
  | otherwise =
      let s = smithSet ballots
       in s : smithRanking (mapBallots (List.\\ s) ballots)

-- | Given ranked ballot results, compute the Borda ranking.  This is the
-- ranking by total Borda count, which is the sum of each candidate's rank on
-- each ballot.
bordaRanking :: RankedBallots -> [Int]
bordaRanking =
  map fst
    . List.sortOn snd
    . Map.toList
    . Map.fromListWith (+)
    . concatMap (\(r, n) -> zipWith (curry (fmap (* n))) r [1 ..])
    . Map.toList

-- | A tactical modification made by a voter to their ranked ballot to optimize
-- their influence on the outcome of a Borda election.  They will move any
-- candidates they consider likely to win to the top or bottom of their ballot
-- (depending on whether they like or dislike that candidate), to maximize the
-- difference in Borda count between them.  Less likely winners will be used
-- to pad the middle of the ballot.
bordaTactics :: [Int] -> [Int] -> [Int]
bordaTactics likelyWinners preferences =
  fst <$> List.sortOn modifiedRank (zip preferences [0 :: Double ..])
  where
    keyPrefs = take 2 (mapMaybe (`List.elemIndex` preferences) likelyWinners)
    width = fromIntegral (maximum keyPrefs - minimum keyPrefs)
    midpoint =
      fromIntegral (sum keyPrefs)
        / fromIntegral (length keyPrefs) ::
        Double
    modifiedRank (c, r) =
      let likelyRank = maybe 100 fromIntegral (List.elemIndex c likelyWinners)
       in 1 / (1 + exp (-(k / likelyRank) * (r - midpoint))) + r / 100
    k = 1 / (width + 1)

-- | Given ranked ballot results, compute the STAR ranking.  This is the
-- result obtained by ordering candidates by their average rating, then
-- conducting a head-to-head runoff between the top two candidates.
starRanking :: Map Int Double -> RankedBallots -> [Int]
starRanking ratings ballots =
  pluralityRanking (mapBallots (filter (`elem` finalists)) ballots)
    ++ rankByRating (Map.filterWithKey (\c _ -> c `notElem` finalists) ratings)
  where
    finalists = take 2 (rankByRating ratings)

-- | Given two rankings, compute the Spearman rank correlation coefficient.
-- This is a measure of how similar the two rankings are, where 1 means they are
-- identical, 0 means they are completely unrelated, and -1 means they are
-- exactly opposite.
rankCorrelation :: [Int] -> [Int] -> Double
rankCorrelation r1 r2 = 1 - (6 * d2) / (n * (n * n - 1))
  where
    n = fromIntegral (length r1)
    m1 = Map.fromList (zip r1 [1 :: Int ..])
    m2 = Map.fromList (zip r2 [1 :: Int ..])
    d2 =
      sum
        [ (fromIntegral (m1 Map.! i) - fromIntegral (m2 Map.! i)) ^ (2 :: Int)
          | i <- [1 .. length r1]
        ]
