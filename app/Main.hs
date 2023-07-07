{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Main where

import Control.Monad (replicateM, when)
import Data.Bool (bool)
import Data.Coerce (coerce)
import Data.Foldable (foldl', traverse_)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector
import Dist (Dist (..), sample)
import Elections
  ( Ballot (Ballot),
    BallotFormat (..),
    Ballots (Ballots),
    VotingMethod (..),
    honest,
    tactical,
    tally,
    winners,
  )
import Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import Graphics.Rendering.Chart.Easy
  ( FillStyle (..),
    HTextAnchor (..),
    RectCornerStyle (RCornerRounded),
    Rectangle (..),
    VTextAnchor (..),
    def,
    goldenrod,
    layout_title,
    liftEC,
    plot,
    plot_annotation_background,
    plot_annotation_hanchor,
    plot_annotation_values,
    plot_annotation_vanchor,
    points,
    withOpacity,
    (.=),
  )
import Model
  ( MixtureOfZipfGaussians (..),
    VoterModel,
    mixtureOfZipfGaussians,
    zipfUniform,
  )
import Options (Options (..), getOptions)
import System.Random (mkStdGen, randomIO, setStdGen)
import System.Random.Shuffle (shuffleM)
import Voting (disutilities, utilityRanking)

-- | A voter model that is a mixture of Zipf-Gaussian sub-populations.  This
-- is the model we'll use for most of our analysis.
makeModel :: IO VoterModel
makeModel =
  mixtureOfZipfGaussians $
    MixtureOfZipfGaussians
      { mogNumDimensions = 100,
        mogNumGaussians = 10,
        mogWeights = Uniform 0 1,
        mogMeans = zipfUniform 3 100,
        mogScales = Uniform 1 2,
        mogDimPeturbation = Gaussian 0 10
      }

-- | Project a high-dimensional vector to 2 dimensions, for plotting.  The
-- first three dimensions are an isomoprphic projection, and the fourth
-- dimension is overlayed in a different direction so we can visualize more of
-- the variation.
projectTo2D :: Vector Double -> (Double, Double)
projectTo2D v = (x + z + w, y + z - w)
  where
    (x, y, z, w) = (v Vector.! 0, v Vector.! 1, v Vector.! 2, v Vector.! 3)

-- | Plot the voters and candidates projected into 2D space.
drawPoints :: [Vector Double] -> [Vector Double] -> IO ()
drawPoints voters candidates = toFile def "points.svg" $ do
  layout_title .= "Voter Model"
  let projectedVoters = projectTo2D <$> voters
  plot (points "voters" projectedVoters)
  let projectedCandidates = projectTo2D <$> candidates
  plot $ liftEC $ do
    plot_annotation_values
      .= zipWith
        (\(x, y) i -> (x, y, show i))
        projectedCandidates
        [1 :: Int ..]
    plot_annotation_hanchor .= HTA_Centre
    plot_annotation_vanchor .= VTA_Centre
    plot_annotation_background
      .= def
        { _rect_fillStyle = Just (FillStyleSolid (withOpacity goldenrod 0.8)),
          _rect_minsize = (10, 10),
          _rect_cornerStyle = RCornerRounded 5
        }

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

-- | Given a list of rankings, compute the percent of the time that each pair
-- of rankings agree on the winner.
winnerAgreement :: [[Int]] -> [[Int]] -> [[Double]]
winnerAgreement rs1 rs2 =
  [[bool 0 1 (head r1 == head r2) | r2 <- rs2] | r1 <- rs1]

-- | Given a list of rankings, compute the Spearman rank correlation between
-- each pair of rankings.
correlations :: [[Int]] -> [[Int]] -> [[Double]]
correlations rs1 rs2 =
  [[rankCorrelation r1 r2 | r2 <- rs2] | r1 <- rs1]

-- | Run a simulation of an election, and return the winner agreement and
-- correlation matrices.
simulate ::
  [Vector Double] ->
  [Vector Double] ->
  IO ([(String, [[Int]])], [[Double]], [[Double]])
simulate voters candidates = do
  let disutil = disutilities candidates <$> voters
      n = length candidates

  -- For tactical voting, we need voters with only partial knowledge about the
  -- strength of candidates.  We'll run a "poll" among a small sample of voters
  -- to get this partial information.
  let polled = take 300 disutil
      pollBallots = tally @SingleVote (honest @SingleVote <$> polled)
      pollResults = winners @Plurality pollBallots [1 .. n]

  -- Now the actual election.
  let utilitarian = utilityRanking voters candidates
      honestSingle = tally @SingleVote (honest @SingleVote <$> disutil)
      honestScoreRank@(Ballots (honestScored, honestRanked)) =
        tally @ScoreImpliesRank
          (coerce . honest @Scored . disutilities candidates <$> voters)

      condorcet = winners @Condorcet honestRanked [1 .. n]
      irv = winners @IRV honestRanked [1 .. n]
      plurality = winners @Plurality honestSingle [1 .. n]
      borda = winners @Borda honestRanked [1 .. n]
      range = winners @Range honestScored [1 .. n]
      star = winners @STAR honestScoreRank [1 .. n]
      tacticalPlurality =
        winners @Plurality
          (tally (tactical @Plurality (concat pollResults) <$> disutil))
          [1 .. n]
      ultraTacticalPlurality =
        winners @Plurality
          (tally (tactical @Plurality (concat tacticalPlurality) <$> disutil))
          [1 .. n]
      tacticalIRV =
        winners @IRV
          (tally (tactical @IRV (concat pollResults) <$> disutil))
          [1 .. n]
      ultraTacticalIRV =
        winners @IRV
          (tally (tactical @IRV (concat tacticalIRV) <$> disutil))
          [1 .. n]
      tacticalBorda =
        winners @Borda
          (tally (tactical @Borda (concat pollResults) <$> disutil))
          [1 .. n]
      ultraTacticalBorda =
        winners @Borda
          (tally (tactical @Borda (concat tacticalBorda) <$> disutil))
          [1 .. n]
      tacticalRange =
        winners @Range
          (tally (tactical @Range (concat pollResults) <$> disutil))
          [1 .. n]
      ultraTacticalRange =
        winners @Range
          (tally (tactical @Range (concat tacticalRange) <$> disutil))
          [1 .. n]
      tacticalStar =
        winners @STAR
          (tally (tactical @STAR (concat pollResults) <$> disutil))
          [1 .. n]
      ultraTacticalStar =
        winners @STAR
          (tally (tactical @STAR (concat tacticalStar) <$> disutil))
          [1 .. n]

  let allRankings =
        [ ("Util", utilitarian),
          ("Cond", condorcet),
          ("nIRV", irv),
          ("tIRV", tacticalIRV),
          ("uIRV", ultraTacticalIRV),
          ("nPlu", plurality),
          ("tPlu", tacticalPlurality),
          ("uPlu", ultraTacticalPlurality),
          ("nBrd", borda),
          ("tBrd", tacticalBorda),
          ("uBrd", ultraTacticalBorda),
          ("nRng", range),
          ("tRng", tacticalRange),
          ("uRng", ultraTacticalRange),
          ("nStr", star),
          ("tStr", tacticalStar),
          ("uStr", ultraTacticalStar)
        ]

  traverse_ (\(lbl, r) -> putStrLn $ lbl <> ": " <> show r) allRankings

  -- Shuffle the order of ties twice, so statistics will on average reflect the
  -- fact that we learned nothing about the order of tied candidates, even in
  -- self-correlation.
  rs1 <- traverse (fmap concat . traverse shuffleM . snd) allRankings
  rs2 <- traverse (fmap concat . traverse shuffleM . snd) allRankings
  pure (allRankings, winnerAgreement rs1 rs2, correlations rs1 rs2)

main :: IO ()
main = do
  options <- getOptions
  results <- replicateM (numTrials options) $ do
    putStrLn "------------------"
    case seed options of
      Nothing -> do
        s <- randomIO
        putStrLn $ "New seed: " <> show s
        setStdGen (mkStdGen s)
      Just s -> do
        putStrLn $ "Reusing seed: " <> show s
        setStdGen (mkStdGen s)

    model <- makeModel
    candidates <- replicateM (numCandidates options) $ sample model
    voters <- replicateM (numVoters options) $ sample model
    when (numTrials options == 1) $ drawPoints (take 2000 voters) candidates
    simulate voters candidates

  let (ranks, rawWinAgree, rawCorr) = unzip3 results
      labels = fst <$> head ranks
      winAgree = foldl' (zipWith (zipWith (+))) (repeat (repeat 0)) rawWinAgree
      corr = foldl' (zipWith (zipWith (+))) (repeat (repeat 0)) rawCorr

  putStrLn "Winner Agreement:"
  putStrLn $ "         " <> List.intercalate "  " labels
  traverse_
    print
    (zip labels (fmap (/ fromIntegral (numTrials options)) <$> winAgree))

  putStrLn "Correlation Matrix:"
  putStrLn $ "         " <> List.intercalate "  " labels
  traverse_
    print
    (zip labels (fmap (/ fromIntegral (numTrials options)) <$> corr))
