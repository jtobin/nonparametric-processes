{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

-- A hilariously over-engineered CRP.

import Control.Applicative
import Control.Error
import Control.Lens
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List
import Options.Applicative
import System.Random.MWC
import Text.Printf

data Restaurant = Restaurant {
    _numberOfTables    :: {-# UNPACK #-} !Int
  , _numberOfCustomers :: {-# UNPACK #-} !Int
  , _seatingPlan       :: !(IntMap Int)
  }

data ProcessParameters = ProcessParameters {
    _observations :: Int
  , _dispersion   :: Double
  }

makeLenses ''Restaurant

instance Show Restaurant where
  show restaurant = intercalate "\n" $ outputMap^..folded where 
    output j x = "table " ++ show j ++ ": " ++ printf "%0.2f" (x :: Double)
    outputMap  = imap output asProbs
    asProbs    = scaleBy (restaurant^.numberOfCustomers)
                     <$> (restaurant^.seatingPlan)

-- | Chinese Restaurant Process.
chineseRestaurantProcess 
  :: PrimMonad m
  => ProcessParameters
  -> Gen (PrimState m)
  -> m Restaurant
chineseRestaurantProcess parameters gen = 
  allCustomersArrive parameters gen `execStateT` emptyRestaurant

-- | All customers arrive.
allCustomersArrive
  :: PrimMonad m
  => ProcessParameters
  -> Gen (PrimState m)
  -> StateT Restaurant m ()
allCustomersArrive (ProcessParameters customers alpha) gen = 
  replicateM_ customers $ newCustomerArrives alpha gen

-- | A single customer arrives.
newCustomerArrives 
  :: PrimMonad m
  => Double
  -> Gen (PrimState m)
  -> StateT Restaurant m ()
newCustomerArrives alpha gen = do
  nc <- use numberOfCustomers
  z  <- lift $ uniform gen
  if   z < newTableProbability nc alpha
  then startNewTable
  else joinExistingTable gen

-- | Start a new table.
startNewTable :: PrimMonad m => StateT Restaurant m ()
startNewTable = do
  numberOfTables    %= succ
  nt <- use numberOfTables
  seatingPlan       %= (at nt ?~ 1)
  numberOfCustomers %= succ

-- | Join an existing table.
joinExistingTable :: PrimMonad m => Gen (PrimState m) -> StateT Restaurant m ()
joinExistingTable gen = do
  restaurant  <- get
  chosenTable <- lift $ sampleFromRestaurant restaurant gen
  seatingPlan       %= (ix chosenTable %~ succ)
  numberOfCustomers %= succ

-- | Sample a table randomly from the restaurant.
sampleFromRestaurant :: PrimMonad m => Restaurant -> Gen (PrimState m) -> m Int
sampleFromRestaurant restaurant gen = do
  z                   <- uniform gen
  let nc               = restaurant^.numberOfCustomers
      tableMasses      = scaleBy nc <$> restaurant^.seatingPlan
      tableCumulative  = scanl1Of traverse (+) tableMasses
      greaterThanZ _ x = x >= (z :: Double)
      selectedTable    = ifind greaterThanZ tableCumulative
      notFoundError    = error "sampleFromRestaurant: not found"

  return . fst $ fromMaybe notFoundError selectedTable

-- | Observe a CRP.
observeProcess :: ProcessParameters -> IO ()
observeProcess parameters = withSystemRandom . asGenIO $ \g -> 
  chineseRestaurantProcess parameters g >>= print

-- | An empty restaurant.
emptyRestaurant :: Restaurant
emptyRestaurant = Restaurant 0 0 IntMap.empty

-- | Evaluate the probability of starting a new table.
newTableProbability :: (Fractional a, Integral b) => b -> a -> a
newTableProbability n a = a / (fromIntegral n + a)

-- | Typecast and scale something by something else.
scaleBy :: (Fractional c, Integral a, Integral b) => b -> a -> c
scaleBy z = (/ fromIntegral z) . fromIntegral

-- | Available command-line options.
commandLineOptions :: Parser ProcessParameters
commandLineOptions = ProcessParameters
  <$> option ( long "customers"
            <> short 'c'
            <> help "Number of customers to observe" )
  <*> option ( long "dispersion"
            <> short 'd'
            <> help "CRP dispersion parameter" )

main :: IO ()
main = execParser opts >>= observeProcess where 
  opts = info (helper <*> commandLineOptions) desc
  desc = fullDesc 
      <> progDesc "Observe a Chinese Restaurant Process."
      <> header "Chinese Restaurant Process"

