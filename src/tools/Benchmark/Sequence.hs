{-# LANGUAGE UndecidableInstances #-}
module Main (

    main

) where

-- Control

import Control.DeepSeq

-- Data

import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Incremental
import           Data.Incremental.Sequence as Incremental.Seq

-- Test

import Test.QuickCheck

-- Benchmark

import Benchmark

main :: IO ()
main = do
    performSortBenchmark 10000 10
    performSortBenchmark 100000 10
    performSortBenchmark 1000000 10

-- * Benchmarking

performSortBenchmark :: Int -> Int -> IO ()
performSortBenchmark = performBenchmark (Seq.sort :: Seq Int -> Seq Int)
                                        Incremental.Seq.sort

performBenchmark :: (Arbitrary a, NFData a, Changeable a,
                     DefaultChange a ~ PrimitiveChange a,
                     Arbitrary b, NFData b, Changeable b)
                 => (Seq a -> Seq b)
                 -> (Seq a ->> Seq b)
                 -> Int
                 -> Int
                 -> IO ()
performBenchmark fun trans initLen changeCount = do
    putStrLn $ "Length of initial sequence: " ++ show initLen
    putStrLn $ "Number of changes:          " ++ show changeCount
    src <- generate $ srcGen initLen changeCount
    runBenchmark fun trans src

-- * Benchmark data generation

srcGen :: (Arbitrary a, Changeable a, DefaultChange a ~ PrimitiveChange a)
       => Int
       -> Int
       -> Gen (Seq a, [DefaultChange (Seq a)])
srcGen initLen changeCount = do
    seq <- seqGen initLen
    changes <- changesGen initLen changeCount
    return (seq, changes) where

    changesGen len changeCount
        | changeCount == 0 = return []
        | otherwise        = do
            (change, lenDiff) <- changeAndLengthDiffGen len
            changes <- changesGen (len + lenDiff) (pred changeCount)
            return (change : changes)

-- | Generates a sequence of the given length.
seqGen :: Arbitrary a => Int -> Gen (Seq a)
seqGen len = do
    elems <- vectorOf len arbitrary
    return (Seq.fromList elems)

{-|
    Generates a sequence change that deals with only a single element and stays
    within the bounds of the sequence, along with the change in length that this
    change causes.
-}
changeAndLengthDiffGen :: (Arbitrary a,
                           Changeable a,
                           DefaultChange a ~ PrimitiveChange a)
                       => Int
                       -> Gen (DefaultChange (Seq a), Int)
changeAndLengthDiffGen len
    | len == 0  = insertGen
    | otherwise = oneof [insertGen, deleteGen, shiftGen, changeAtGen] where

    insertGen = do
        ix <- choose (0, len)
        elem <- arbitrary
        return (insert ix (Seq.singleton elem), 1)

    deleteGen = do
        ix <- choose (0, pred len)
        return (delete ix 1, -1)

    shiftGen = do
        src <- choose (0, pred len)
        tgt <- choose (0, pred len)
        return (shift src 1 tgt, 0)

    changeAtGen = do
        ix <- choose (0, pred len)
        newElem <- arbitrary
        return (changeAt ix (ReplaceBy newElem), 0)

-- * Evaluation of changes

instance (NFData a, NFData (DefaultChange a)) => NFData (AtomicChange a) where

    rnf (Insert ix seq)      = ix `deepseq` seq `deepseq` ()
    rnf (Delete ix len)      = ix `deepseq` len `deepseq` ()
    rnf (Shift src len tgt)  = src `deepseq` len `deepseq` tgt `deepseq` ()
    rnf (ChangeAt ix change) = ix `deepseq` change `deepseq` ()
