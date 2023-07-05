module Options where

import Options.Applicative

data Options = Options
  { numVoters :: Int,
    numCandidates :: Int,
    numTrials :: Int,
    seed :: Maybe Int
  }

options :: Parser Options
options =
  Options
    <$> option
      auto
      ( long "voters"
          <> short 'v'
          <> metavar "N"
          <> help "Number of voters"
          <> showDefault
          <> value 1000
      )
    <*> option
      auto
      ( long "candidates"
          <> short 'c'
          <> metavar "N"
          <> help "Number of candidates"
          <> showDefault
          <> value 5
      )
    <*> option
      auto
      ( long "trials"
          <> short 't'
          <> metavar "N"
          <> help "Number of trials"
          <> showDefault
          <> value 1
      )
    <*> optional
      ( option
          auto
          ( long "seed"
              <> short 's'
              <> metavar "SEED"
              <> help "Random seed"
          )
      )

getOptions :: IO Options
getOptions =
  execParser $
    info
      (helper <*> options)
      ( fullDesc
          <> progDesc "Run simulations of elections using spatial models"
          <> header "spatial-voting - a spatial voting simulator"
      )