{-# LANGUAGE GADTs #-}

module Dist where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Vector.Unboxed (Unbox, Vector)
import Data.Vector.Unboxed qualified as Vector
import System.Random (randomRIO)

-- | A probablility distribution over a type `t`.
data Dist t where
  Constant :: !t -> Dist t
  Gaussian :: {-# UNPACK #-} !Double -> {-# UNPACK #-} !Double -> Dist Double
  Uniform :: {-# UNPACK #-} !Double -> {-# UNPACK #-} !Double -> Dist Double
  VectorOf :: Unbox t => [Dist t] -> Dist (Vector t)
  Mixture :: NonEmpty (Double, Dist t) -> Dist t

-- | Sample from a distribution.
sample :: Dist t -> IO t
sample (Constant x) = pure x
sample (Uniform lo hi) = randomRIO (lo, hi)
sample (Gaussian mu sigma) = do
  u1 <- randomRIO (0, 1)
  u2 <- randomRIO (0, 1)
  let r = sqrt (-2 * log u1)
      theta = 2 * pi * u2
  pure $ mu + sigma * r * cos theta
sample (VectorOf ds) = Vector.generateM (length ds) (sample . (ds !!))
sample (Mixture ((w, d) :| ds)) = case NonEmpty.nonEmpty ds of
  Nothing -> sample d
  Just ds' -> do
    x <- randomRIO (0, 1)
    if x < (w / (w + sum (fst <$> ds')))
      then sample d
      else sample (Mixture ds')
