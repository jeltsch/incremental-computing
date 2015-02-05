module TestSuite.Sequence (

    tests

) where

-- Data

import           Data.Foldable (asum)
import           Data.Incremental
import           Data.Sequence (Seq)
import qualified Data.Sequence             as Seq
import qualified Data.Incremental.Sequence as IncSeq

-- Test

import Test.QuickCheck
import Test.QuickCheck.Poly

-- Distribution

import Distribution.TestSuite
import Distribution.TestSuite.QuickCheck

-- TestSuite
import TestSuite

-- * List of tests

tests :: IO [Test]
tests = return [
            toFunctionTest
        ]

-- * Individual tests

toFunctionTest :: Test
toFunctionTest = testGroup "toFunction" $
                 [
                    mapTest,
                    map'Test,
                    concatTest,
                    singletonTest,
                    gateTest,
                    gate'Test,
                    filterTest,
                    filter'Test,
                    reverseTest
                 ] where

    mapTest :: Test
    mapTest = testProperty "toFunction on map" prop where

        prop :: Seq A -> Bool
        prop seq = toFunction (IncSeq.map testTrans) seq ==
                   fmap (toFunction testTrans) seq

    map'Test :: Test
    map'Test = testProperty "toFunction on map'" prop where

        prop :: Seq C -> Bool
        prop seq = toFunction (IncSeq.map' testFun) seq == fmap testFun seq

    concatTest :: Test
    concatTest = testProperty "toFunction on concat" prop where

        prop :: Seq (Seq A) -> Bool
        prop seq = toFunction IncSeq.concat seq == concatSeq seq

    singletonTest :: Test
    singletonTest = testProperty "toFunction on singleton" prop where

        prop :: A -> Bool
        prop elem = toFunction IncSeq.singleton elem == Seq.singleton elem

    gateTest :: Test
    gateTest = testProperty "toFunction on gate" prop where

        prop :: A -> Bool
        prop val = toFunction (IncSeq.gate testPrdTrans) val ==
                   gateSeq (toFunction testPrdTrans) val

    gate'Test :: Test
    gate'Test = testProperty "toFunction on gate'" prop where

        prop :: C -> Bool
        prop val = toFunction (IncSeq.gate' testPrdFun) val ==
                   gateSeq testPrdFun val

    filterTest :: Test
    filterTest = testProperty "toFunction on filter" prop where

        prop :: Seq A -> Bool
        prop seq = toFunction (IncSeq.filter testPrdTrans) seq ==
                   Seq.filter (toFunction testPrdTrans) seq

    filter'Test :: Test
    filter'Test = testProperty "toFunction on filter'" prop where

        prop :: Seq C -> Bool
        prop seq = toFunction (IncSeq.filter' testPrdFun) seq ==
                   Seq.filter testPrdFun seq

    reverseTest :: Test
    reverseTest = testProperty "toFunction on reverse" prop where

        prop :: Seq A -> Bool
        prop seq = toFunction IncSeq.reverse seq == Seq.reverse seq

concatSeq :: Seq (Seq a) -> Seq a
concatSeq = asum

gateSeq :: (a -> Bool) -> a -> Seq a
gateSeq prd val | prd val   = Seq.singleton val
                | otherwise = Seq.empty
