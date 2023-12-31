module Model where

import Control.Monad (replicateM)
import Data.Function (on)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Traversable (for)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector
import Dist (Dist (..), sample)

-- | A voter (or candidate) is a vector of real numbers, representing their
-- opinions or preferences on a number of axes, which might represent issues,
-- cultural affiliations, or other factors.
type VoterModel = Dist (Vector Double)

-- | A fixed uniform model assumes that voter preferences are uniformly and
-- independently distributed across some number of axes, each of which is in
-- general equally important.
--
-- This is a veru simple model, which won't capture the complexity of real
-- elections, but it's a good starting point.
fixedUniform :: Int -> VoterModel
fixedUniform k = VectorOf (replicate k (Uniform 0 1))

-- | A Zipf-uniform model assumes that voter preferences are distributed
-- uniformly along various axes, but that the axes are not equally important.
-- Instead, their importance follows a Zipf distribution, where later axes
-- have diminishing importance proportional to the inverse of number of
-- dimensions.
zipfUniform :: Double -> Int -> VoterModel
zipfUniform scale n =
  VectorOf
    [ Uniform (-scale / fromIntegral i) (scale / fromIntegral i)
      | i <- [1 .. n]
    ]

-- | A Zipf-Gaussian model assumes that voter preferences are distributed
-- along various axes according to a Zipf distribution, but that voter
-- preferences are normally distributed around the mean of each axis, with more
-- voters near the center, and fewer voters near the extremes.
zipfGaussian :: Vector Double -> VoterModel
zipfGaussian mus =
  VectorOf
    [ Gaussian mu (1 / fromIntegral i)
      | (i, mu) <- zip [1 :: Int ..] (Vector.toList mus)
    ]

-- | The MoZG, or Mixture of Zipf-Gaussians, model modifies the Zipf-Gaussian
-- model by introducing multiple sub-populations, each with a different size,
-- mean position, degree of spread, and their own characteristics about the
-- amount of variation on each axis of political affinity.
--
-- This model is intended to capture phenomena we see in actual voting
-- populations, such as grouping into political parties or affiliations,
-- regional and ethnic differences, etc.  Each group might not only have
-- different opinions, but also different levels of importance or variation in
-- their opinions of different types.
data MixtureOfZipfGaussians = MixtureOfZipfGaussians
  { mogNumDimensions :: Int,
    mogNumGaussians :: Int,
    mogWeights :: Dist Double,
    mogMeans :: Dist (Vector Double),
    mogScales :: Dist Double,
    mogDimPeturbation :: Dist Double
  }

-- | Shuffles a list, but only slightly, so that the relative order of elements
-- is mostly preserved, but not entirely.
--
-- This is used in the MoZG model to perturb the order of dimensions, so that
-- there is still a global ordering in terms of amount of variation, but each
-- group applies local variation on top of this global order.
shuffleSlightly :: Dist Double -> [a] -> IO [a]
shuffleSlightly offset xs = do
  positions <- for [1 .. length xs] $
    \i -> (+ fromIntegral i) <$> sample offset
  pure (fst <$> List.sortOn snd (zip xs positions))

-- | Generates a voter model from MoZG model hyper-parameters.
mixtureOfZipfGaussians :: MixtureOfZipfGaussians -> IO VoterModel
mixtureOfZipfGaussians mog = do
  let n = mogNumGaussians mog
      k = mogNumDimensions mog
  maybe (Constant (Vector.replicate k 0)) Mixture . NonEmpty.nonEmpty
    <$> replicateM
      n
      ( makeGaussian
          <$> sample (mogWeights mog)
          <*> sample (mogMeans mog)
          <*> sample (mogScales mog)
          <*> shuffleSlightly (mogDimPeturbation mog) [1 .. k]
      )
  where
    makeGaussian w m s dims =
      ( w,
        VectorOf
          [ Gaussian mu (s / fromIntegral i)
            | (mu, i) <- zip (Vector.toList m) dims
          ]
      )

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

type Candidate = Int

-- | The result of adjusting candidate ballots.  This is a step in several
-- voting processes, for instance when a candidate is eliminated.
mapRanks ::
  Num a =>
  ([Candidate] -> [Candidate]) ->
  Map [Candidate] a ->
  Map [Candidate] a
mapRanks f = Map.delete [] . Map.mapKeysWith (+) f

-- | A ranking of candidates by utility.  This isn't a realistic voting model,
-- but it's a useful tool for comparing voting systems to see how well they
-- reflect the actual preferences of voters.
utilityRanking :: [Vector Double] -> [Vector Double] -> [[Candidate]]
utilityRanking voters candidates =
  fmap (fmap fst) $
    List.groupBy ((==) `on` snd) $
      List.sortOn
        snd
        [ (i, sum (disutility c <$> voters))
          | (i, c) <- zip [1 ..] candidates
        ]
