module Main where

import Control.Monad (replicateM, when)
import Data.Bool (bool)
import Data.Foldable (foldl', traverse_)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector
import Dist (Dist (..), sample)
import Elections
  ( bordaRanking,
    bordaTactics,
    dual,
    generalizedIrvRanking,
    irvRanking,
    pluralityRanking,
    pluralityTactics,
    rangeRating,
    rankCorrelation,
    smithRanking,
    starRanking,
    utilityRanking,
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
import System.Random (setStdGen, randomIO, mkStdGen)
import Voting (mapBallots, rankByRating, rankedBallots)

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

-- | Given a list of rankings, compute the percent of the time that each pair
-- of rankings agree on the winner.
winnerAgreement :: [[Int]] -> [[Double]]
winnerAgreement rankings =
  [[bool 0 1 (head r1 == head r2) | r2 <- rankings] | r1 <- rankings]

-- | Given a list of rankings, compute the Spearman rank correlation between
-- each pair of rankings.
correlations :: [[Int]] -> [[Double]]
correlations rankings =
  [[rankCorrelation r1 r2 | r2 <- rankings] | r1 <- rankings]

-- | Run a simulation of an election, and return the winner agreement and
-- correlation matrices.
simulate :: [Vector Double] -> [Vector Double] -> IO ([[Double]], [[Double]])
simulate voters candidates = do
  -- For tactical voting, we need voters with only partial knowledge about the
  -- strength of candidates.  We'll run a "poll" among a small sample of voters
  -- to get this partial information.
  let pollSize = round (sqrt (fromIntegral (length voters)) :: Double)
      pollBallots = rankedBallots (take pollSize voters) candidates
      pollResults = pluralityRanking pollBallots

  -- Now the actual election.  We'll use the same voters, but with full
  -- knowledge of their preferences.
  let ballots = rankedBallots voters candidates
      utilitarian = utilityRanking voters candidates
      smith = smithRanking ballots
      irv = irvRanking ballots
      plurality = pluralityRanking ballots
      tacticalPlurality =
        pluralityRanking (mapBallots (pluralityTactics pollResults) ballots)
      borda = bordaRanking ballots
      tacticalBorda =
        bordaRanking (mapBallots (bordaTactics pollResults) ballots)
      honestRating = rangeRating Nothing voters candidates
      honestRange = rankByRating honestRating
      tacticalRating = rangeRating (Just (0.5, pollResults)) voters candidates
      tacticalRange = rankByRating tacticalRating
      honestStar = starRanking honestRating ballots
      tacticalStar = starRanking tacticalRating ballots
      dualIrv = generalizedIrvRanking (last . dual pluralityRanking) ballots
      dualPlurality = dual pluralityRanking ballots

  putStrLn $ "Utilitarian: " <> show utilitarian
  putStrLn $ "Condorcet/Smith: " <> show smith
  putStrLn $ "IRV: " <> show irv
  putStrLn $ "Plurality: " <> show plurality
  putStrLn $ "Plurality tactical: " <> show tacticalPlurality
  putStrLn $ "Borda: " <> show borda
  putStrLn $ "Borda tactical: " <> show tacticalBorda
  putStrLn $ "Range: " <> show honestRange
  putStrLn $ "Range tactical: " <> show tacticalRange
  putStrLn $ "STAR: " <> show honestStar
  putStrLn $ "STAR tactical: " <> show tacticalStar
  putStrLn $ "Dual IRV: " <> show dualIrv
  putStrLn $ "Dual Plurality: " <> show dualPlurality

  let allRankings =
        [ utilitarian,
          concat smith,
          irv,
          plurality,
          tacticalPlurality,
          borda,
          tacticalBorda,
          honestRange,
          tacticalRange,
          honestStar,
          tacticalStar,
          dualIrv,
          dualPlurality
        ]
  pure (winnerAgreement allRankings, correlations allRankings)

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

  let (rawWinAgree, rawCorr) = unzip results
      winAgree = foldl' (zipWith (zipWith (+))) (repeat (repeat 0)) rawWinAgree
      corr = foldl' (zipWith (zipWith (+))) (repeat (repeat 0)) rawCorr

  putStrLn "Winner Agreement:"
  traverse_ print (fmap (/ fromIntegral (numTrials options)) <$> winAgree)

  putStrLn "Correlation Matrix:"
  traverse_ print (fmap (/ fromIntegral (numTrials options)) <$> corr)
