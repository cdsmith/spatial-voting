{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Elections where

import Data.Coerce (coerce)
import Data.Foldable (foldl', maximumBy, minimumBy)
import Data.Function (on)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..), comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import Model (Candidate, mapRanks)

data BallotFormat = SingleVote | MultiVote | Ranked | Scored | ScoreImpliesRank
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

newtype Ballot (fmt :: BallotFormat) = Ballot (BallotRep fmt)

newtype Ballots (fmt :: BallotFormat) = Ballots (BallotsRep fmt)

class Tally (fmt :: BallotFormat) where
  type BallotRep fmt
  type BallotsRep fmt
  honest :: Map Candidate Double -> Ballot fmt
  tally :: [Ballot fmt] -> Ballots fmt

instance Tally SingleVote where
  type BallotRep SingleVote = Candidate
  type BallotsRep SingleVote = Map Candidate Int
  honest disutil =
    Ballot $
      fst (minimumBy (compare `on` snd) (Map.toList disutil))
  tally = Ballots . Map.fromListWith (+) . map (,1) . coerce

instance Tally MultiVote where
  type BallotRep MultiVote = Set Candidate
  type BallotsRep MultiVote = Map Candidate Int
  honest disutil = Ballot $ Map.keysSet (Map.filter (< mid) disutil)
    where
      mid = (minimum disutil + maximum disutil) / 2
  tally = Ballots . Map.fromListWith (+) . fmap (,1) . concatMap (Set.toList . coerce)

instance Tally Ranked where
  type BallotRep Ranked = [Candidate]
  type BallotsRep Ranked = Map [Candidate] Int
  honest disutil = Ballot $ fst <$> List.sortOn snd (Map.toList disutil)
  tally = Ballots . Map.fromListWith (+) . fmap (,1) . coerce

instance Tally Scored where
  type BallotRep Scored = Map Candidate Double
  type BallotsRep Scored = Map Candidate Double
  honest disutil = Ballot (adjust <$> disutil)
    where
      near = minimum disutil
      far = maximum disutil
      adjust x = (far - x) / (far - near)
  tally = Ballots . Map.unionsWith (+) . coerce @_ @[_]

instance Tally ScoreImpliesRank where
  type BallotRep ScoreImpliesRank = Map Candidate Double
  type BallotsRep ScoreImpliesRank = (Ballots Scored, Ballots Ranked)
  honest = coerce . honest @Scored
  tally ballots =
    Ballots
      ( tally (coerce ballots),
        tally
          ( Ballot
              . fmap fst
              . List.sortOn @(Down Double) (Down . snd)
              . Map.toList
              <$> coerce ballots
          )
      )

data VotingMethod = Plurality | IRV | Borda | Approval | Range | STAR | Condorcet
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

class Tally (BallotFor m) => Votable (m :: VotingMethod) where
  type BallotFor m :: BallotFormat
  winners :: Ballots (BallotFor m) -> [Candidate] -> [[Candidate]]
  tactical ::
    [Candidate] ->
    Map Candidate Double ->
    Ballot (BallotFor m)

instance Votable Plurality where
  type BallotFor Plurality = SingleVote

  winners (Ballots ballots) candidates =
    fmap fst
      <$> List.groupBy
        ((==) `on` snd)
        ( List.sortOn
            (Down . snd)
            [(c, fromMaybe 0 (Map.lookup c ballots)) | c <- candidates]
        )

  tactical poll disutil =
    Ballot $ minimumBy (comparing (disutil Map.!)) (take 2 poll)

instance Votable IRV where
  type BallotFor IRV = Ranked

  winners (Ballots ballots) candidates
    | Map.null ballots = []
    | otherwise =
        winners @IRV
          (Ballots (mapRanks (List.\\ elim) ballots))
          (candidates List.\\ elim)
          ++ [elim]
    where
      firstPlace = Map.mapKeysWith (+) head ballots
      unranked = candidates List.\\ Map.keys firstPlace
      elim
        | null unranked = Map.keys (Map.filter (== minimum firstPlace) firstPlace)
        | otherwise = unranked

  tactical poll disutil = Ballot (champion : others)
    where
      champion = minimumBy (comparing (disutil Map.!)) (take 3 poll)
      others = List.sortOn (disutil Map.!) (List.delete champion poll)

instance Votable Borda where
  type BallotFor Borda = Ranked

  winners (Ballots ballots) _ =
    map (map fst)
      . List.groupBy ((==) `on` snd)
      . List.sortOn snd
      . Map.toList
      . Map.fromListWith (+)
      . concatMap (\(r, n) -> zipWith (curry (fmap (* n))) r [1 ..])
      . Map.toList
      $ ballots

  tactical poll disutil = Ballot (champion : others ++ [nemesis])
    where
      champion = minimumBy (comparing (disutil Map.!)) (take 2 poll)
      nemesis = maximumBy (comparing (disutil Map.!)) (take 2 poll)
      others =
        List.sortOn
          (disutil Map.!)
          (List.delete champion (List.delete nemesis poll))

instance Votable Approval where
  type BallotFor Approval = MultiVote

  winners (Ballots ballots) candidates =
    fmap fst
      <$> List.groupBy
        ((==) `on` snd)
        ( List.sortOn
            (Down . snd)
            ( Map.toList ballots
                ++ [(c, 0) | c <- candidates, not (c `Map.member` ballots)]
            )
        )

  tactical poll disutil =
    Ballot (Map.keysSet (Map.filter (<= championDisutil) disutil))
    where
      championDisutil = minimum ((disutil Map.!) <$> take 2 poll)

instance Votable Range where
  type BallotFor Range = Scored

  winners (Ballots ballots) _ =
    fmap fst
      <$> List.groupBy
        ((==) `on` snd)
        (List.sortOn (Down . snd) (Map.toList ballots))

  tactical poll disutil =
    Ballot (tacticalScore <$> coerce (honest @Scored disutil))
    where
      likely = Map.filterWithKey (\c _ -> c `elem` take 2 poll) disutil
      champion = minimum likely
      nemesis = maximum likely
      tacticalScore x
        | champion == nemesis = x
        | otherwise = epsilon * x + (1 - epsilon) * scaled
        where
          scaled = max 0 $ min 1 $ (x - nemesis) / (champion - nemesis)
          epsilon = 1e-3

instance Votable STAR where
  type BallotFor STAR = ScoreImpliesRank

  winners (Ballots (scored, Ballots ranked)) candidates =
    winners
      @Plurality
      (Ballots (Map.mapKeysWith (+) (head . filter (`elem` finalists)) ranked))
      finalists
      ++ filter (not . null) (filter (`notElem` finalists) <$> prelims)
    where
      prelims = winners @Range scored candidates
      finalists = take 2 (concat prelims)

  tactical poll disutil = coerce (tactical @Range poll disutil)

instance Votable Condorcet where
  type BallotFor Condorcet = Ranked

  winners (Ballots ballots) candidates
    | Map.null ballots = []
    | otherwise =
        let s = smithSet ballots
         in s
              : winners @Condorcet
                (Ballots (mapRanks (List.\\ s) ballots))
                (candidates List.\\ s)

  -- For this variant of Condorcet voting, where Smith sets are always reported
  -- as ties, there is no tactical voting that can do better than some ordering
  -- of the ties.
  tactical _ = honest @Ranked

-- | Given ranked ballot results, compute the head-to-head result of a pair of
-- candidates.  This is the percent of voters who prefer the first candidate to
-- the second.
headToHead :: Real a => Map [Candidate] a -> Int -> Int -> Double
headToHead ballots c c' =
  uncurry (/) $
    foldl'
      (\(w1, n1) (w2, n2) -> (w1 + w2, n1 + n2))
      (0, 0)
      [ if List.elemIndex c r < List.elemIndex c' r
          then (val, val)
          else (0, val)
        | r <- Map.keys ballots,
          let val = realToFrac (ballots Map.! r)
      ]

-- | Given ranked ballot results, compute the head-to-head matrix of all pairs
-- of candidates.  This is the percent of voters who prefer each candidate to
-- each other candidate.
h2hMatrix :: Real a => Map [Candidate] a -> [[Double]]
h2hMatrix ballots = [[headToHead ballots c c' | c' <- [1 .. n]] | c <- [1 .. n]]
  where
    n = maximum (head $ Map.keys ballots) + 1

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
smithSet :: Real a => Map [Candidate] a -> [Int]
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
