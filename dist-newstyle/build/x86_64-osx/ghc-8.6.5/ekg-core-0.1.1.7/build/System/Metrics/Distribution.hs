{-# LINE 1 "System/Metrics/Distribution.hsc" #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}



-- | This module defines a type for tracking statistics about a series
-- of events. An event could be handling of a request and the value
-- associated with the event -- the value you'd pass to 'add' -- could
-- be the amount of time spent serving that request (e.g. in
-- milliseconds). All operations are thread safe.
module System.Metrics.Distribution
    ( Distribution
    , new
    , add
    , addN
    , read
    , reset

      -- * Gathered statistics
    , Stats
    , mean
    , variance
    , count
    , sum
    , min
    , max
    ) where

import Control.Monad (forM_, replicateM)
import Data.Int (Int64)
import Foreign.C.Types (CInt)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(alignment, peek, poke, sizeOf), peekByteOff,
                         pokeByteOff)
import Prelude hiding (max, min, read, sum)

import Data.Array
import System.Metrics.Distribution.Internal (Stats(..))
import System.Metrics.ThreadId

-- | An metric for tracking events.
newtype Distribution = Distribution { unD :: Array Stripe }

data Stripe = Stripe
    { stripeFp    :: !(ForeignPtr CDistrib)
    }

data CDistrib = CDistrib
    { cCount      :: !Int64
    , cMean       :: !Double
    , cSumSqDelta :: !Double
    , cSum        :: !Double
    , cMin        :: !Double
    , cMax        :: !Double
    , cLock       :: !Int64  -- ^ 0 - unlocked, 1 - locked
    }

instance Storable CDistrib where
    sizeOf _ = ((56))
{-# LINE 63 "System/Metrics/Distribution.hsc" #-}
    alignment _ = alignment (undefined :: CInt)

    peek p = do
        cCount <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p
{-# LINE 67 "System/Metrics/Distribution.hsc" #-}
        cMean <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
{-# LINE 68 "System/Metrics/Distribution.hsc" #-}
        cSumSqDelta <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) p
{-# LINE 69 "System/Metrics/Distribution.hsc" #-}
        cSum <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) p
{-# LINE 70 "System/Metrics/Distribution.hsc" #-}
        cMin <- ((\hsc_ptr -> peekByteOff hsc_ptr 32)) p
{-# LINE 71 "System/Metrics/Distribution.hsc" #-}
        cMax <- ((\hsc_ptr -> peekByteOff hsc_ptr 40)) p
{-# LINE 72 "System/Metrics/Distribution.hsc" #-}
        cLock <- ((\hsc_ptr -> peekByteOff hsc_ptr 48)) p
{-# LINE 73 "System/Metrics/Distribution.hsc" #-}
        return $! CDistrib
            { cCount      = cCount
            , cMean       = cMean
            , cSumSqDelta = cSumSqDelta
            , cSum        = cSum
            , cMin        = cMin
            , cMax        = cMax
            , cLock       = cLock
            }

    poke p CDistrib{..} = do
        ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p cCount
{-# LINE 85 "System/Metrics/Distribution.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) p cMean
{-# LINE 86 "System/Metrics/Distribution.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 16)) p cSumSqDelta
{-# LINE 87 "System/Metrics/Distribution.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 24)) p cSum
{-# LINE 88 "System/Metrics/Distribution.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 32)) p cMin
{-# LINE 89 "System/Metrics/Distribution.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 40)) p cMax
{-# LINE 90 "System/Metrics/Distribution.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 48)) p cLock
{-# LINE 91 "System/Metrics/Distribution.hsc" #-}

newCDistrib :: IO (ForeignPtr CDistrib)
newCDistrib = do
    fp <- mallocForeignPtr
    withForeignPtr fp $ \ p -> poke p $ CDistrib
        { cCount      = 0
        , cMean       = 0.0
        , cSumSqDelta = 0.0
        , cSum        = 0.0
        , cMin        = 0.0
        , cMax        = 0.0
        , cLock       = 0
        }
    return fp

newStripe :: IO Stripe
newStripe = do
    fp <- newCDistrib
    return $! Stripe
        { stripeFp    = fp
        }

-- | Number of lock stripes. Should be greater or equal to the number
-- of HECs.
numStripes :: Int
numStripes = 8

-- | Get the stripe to use for this thread.
myStripe :: Distribution -> IO Stripe
myStripe distrib = do
    tid <- myCapability
    return $! unD distrib `index` (tid `mod` numStripes)

------------------------------------------------------------------------
-- Exposed API

-- | Create a new distribution.
new :: IO Distribution
new = (Distribution . fromList numStripes) `fmap`
      replicateM numStripes newStripe

-- | Add a value to the distribution.
add :: Distribution -> Double -> IO ()
add distrib val = addN distrib val 1

foreign import ccall unsafe "hs_distrib_add_n" cDistribAddN
    :: Ptr CDistrib -> Double -> Int64 -> IO ()

-- | Add the same value to the distribution N times.
addN :: Distribution -> Double -> Int64 -> IO ()
addN distrib val n = do
    stripe <- myStripe distrib
    withForeignPtr (stripeFp stripe) $ \ p ->
        cDistribAddN p val n

foreign import ccall unsafe "hs_distrib_combine" combine
    :: Ptr CDistrib -> Ptr CDistrib -> IO ()

-- | Get the current statistical summary for the event being tracked.
read :: Distribution -> IO Stats
read distrib = do
    result <- newCDistrib
    CDistrib{..} <- withForeignPtr result $ \ resultp -> do
        forM_ (toList $ unD distrib) $ \ stripe ->
            withForeignPtr (stripeFp stripe) $ \ p ->
            combine p resultp
        peek resultp
    return $! Stats
        { mean  = cMean
        , variance = if cCount == 0 then 0.0
                     else cSumSqDelta / fromIntegral cCount
        , count = cCount
        , sum   = cSum
        , min   = cMin
        , max   = cMax
        }

foreign import ccall unsafe "hs_distrib_zero" zero
    :: Ptr CDistrib -> IO ()

reset :: Distribution -> IO ()
reset distrib = do
    forM_ (toList $ unD distrib) $ \ stripe ->
        withForeignPtr (stripeFp stripe) $ \ p ->
          zero p
