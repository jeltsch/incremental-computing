module Benchmark (

    runBenchmark

) where

-- Control

import Control.Monad
import Control.DeepSeq
import Control.Exception

-- Data

import           Data.Foldable (toList)
import           Data.MultiChange (MultiChange)
import qualified Data.MultiChange as MultiChange
import           Data.Incremental

-- System

import System.CPUTime

-- Test

import Test.QuickCheck

-- Utilities

import Utilities

{-FIXME:
    Measure cumulative times. For doing so, perform first all recomputations and
    then all adaptions. In each case, measure the start time and the time after
    each step.
-}

-- * Benchmarking

runBenchmark :: (NFData a, Changeable a, NFData (DefaultChange a),
                 NFData b, Changeable b)
             => (a -> b)
             -> (a ->> b)
             -> (a, [DefaultChange a])
             -> IO ()
runBenchmark fun trans src = do
    timePairs <- benchmark fun trans src
    zipWithM_ outputTimePair [0 ..] timePairs

data TimePair = TimePair {
                    recomputeTime :: Integer,
                    adaptTime     :: Integer
                }

benchmark :: (NFData a, Changeable a, NFData (DefaultChange a),
              NFData b, Changeable b)
          => (a -> b)
          -> (a ->> b)
          -> (a, [DefaultChange a])
          -> IO [TimePair]
benchmark fun trans src = do
    fullyEvaluatedSrc <- fullyEvaluate src
    let recomputeResults = recompute fun fullyEvaluatedSrc
    let adaptResults = adapt trans fullyEvaluatedSrc
    recomputeTimes <- getTimes recomputeResults
    adaptTimes <- getTimes adaptResults
    return $ zipWith TimePair recomputeTimes adaptTimes

getTimes :: NFData b => [b] -> IO [Integer]
getTimes results = do
    startTime <- getCPUTime
    let getTime result = do
            fullyEvaluate result
            finishTime <- getCPUTime
            return (finishTime - startTime)
    mapM getTime results

fullyEvaluate :: NFData a => a -> IO a
fullyEvaluate = evaluate . force

outputTimePair :: Int -> TimePair -> IO ()
outputTimePair stepNo timePair = putStrLn $
                                 align stepNoWidth stepNo            ++
                                 ": R "                              ++
                                 timeOutput (recomputeTime timePair) ++
                                 ", A "                              ++
                                 timeOutput (adaptTime timePair)

timeOutput :: Integer -> String
timeOutput time = align timeIntegralWidth (inCentiseconds `div` 100) ++
                  "."                                                ++
                  show (fractionalPart `div` 10)                     ++
                  show (fractionalPart `mod` 10)                     ++
                  " s" where

    inCentiseconds = (time + 5000000000) `div` 10000000000

    fractionalPart = inCentiseconds `mod` 100

align :: Show a => Int -> a -> String
align width val = Prelude.reverse $
                  take width      $
                  Prelude.reverse (show val) ++ repeat ' '

stepNoWidth :: Int
stepNoWidth = 8

timeIntegralWidth :: Int
timeIntegralWidth = 4

-- * Evaluation of changes

instance NFData a => NFData (PrimitiveChange a) where

    rnf Keep            = ()
    rnf (ReplaceBy val) = val `deepseq` ()

instance NFData p => NFData (MultiChange p) where

    rnf change = rnf (toList change)
