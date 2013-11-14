{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Data.List
import Data.Monoid
import Data.Sequence (Seq)
import Options.Applicative
import Statistics.Distribution
import Statistics.Distribution.Beta
import System.Random.MWC
import Text.Printf

-- | Cord is more appropriate than the historical word for 'bundle of sticks'..
data Cord = Cord {
    _sumOfLogBetas :: {-# UNPACK #-} !Double
  , _sticks        :: !(Seq Double)
  }

data ProcessParameters = ProcessParameters {
    _numberOfTwigs :: Int
  , _dispersion    :: Double
  }

makeLenses ''Cord
makeLenses ''ProcessParameters

instance Show Cord where
  show c = intercalate "\n" lengths where 
    lengths = [ printf "stick %d: %0.4f" j x
              | (j, x) <- zip ([1..] :: [Int]) (c^.sticks^..folded) ]

-- | The stick-breaking process.
stickBreakingProcess 
  :: PrimMonad m
  => ProcessParameters
  -> Gen (PrimState m)
  -> m Cord
stickBreakingProcess ps g = breakSticks ps g `execStateT` wholeStick

-- | Break a bunch of sticks.
breakSticks
  :: PrimMonad m
  => ProcessParameters
  -> Gen (PrimState m)
  -> StateT Cord m ()
breakSticks parameters = replicateM_ n . snap a where
  n = parameters^.numberOfTwigs
  a = parameters^.dispersion

-- | Break a single stick.
snap :: PrimMonad m => Double -> Gen (PrimState m) -> StateT Cord m ()
snap a g = do
  z              <- lift $ generateBetaWithScale a g
  lbp            <- use sumOfLogBetas
  let stick = exp $ log z + lbp
  when (reasonable stick) $
    sticks <>= pure stick
  sumOfLogBetas  += log (1 - z)

-- | Observe a stick-breaking process.
observeProcess :: ProcessParameters -> IO ()
observeProcess parameters = withSystemRandom . asGenIO $ 
  stickBreakingProcess parameters >=> print

-- | Generate a Beta(1, a) variate.
generateBetaWithScale :: PrimMonad m => Double -> Gen (PrimState m) -> m Double
generateBetaWithScale = genContVar . betaDistr 1

-- | Unbroken stick.
wholeStick :: Cord
wholeStick = Cord 0 mempty

-- | Round to a specified number of digits.
roundTo :: Int -> Double -> Double
roundTo n f = fromIntegral (round $ f * (10 ^ n) :: Int) / (10.0 ^^ n)

-- | Squint a bit.
eyeball :: Double -> Double
eyeball = roundTo 4

-- | Don't want to count splinters.
reasonable :: Double -> Bool
reasonable stick = eyeball stick /= 0

-- | Available command-line options.
commandLineOptions :: Parser ProcessParameters
commandLineOptions = ProcessParameters
  <$> option ( long "twigs"
            <> short 'n'
            <> help "Number of twigs to snap" )
  <*> option ( long "dispersion"
            <> short 'a'
            <> help "Dispersion parameter" )

main :: IO ()
main = execParser opts >>= observeProcess where 
  opts = info (helper <*> commandLineOptions) desc
  desc = fullDesc 
      <> progDesc "Observe a stick-breaking process."
      <> header "Stick Breaking Process"

