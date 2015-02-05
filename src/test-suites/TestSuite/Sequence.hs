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

-- TestSuite
import TestSuite

-- * List of tests

tests :: IO [Test]
tests = return [toFunctionTests]

-- * Individual tests

toFunctionTests :: Test
toFunctionTests = testGroup "toFunction" [
        toFunctionTest "map"       (IncSeq.map testTrans)
                                   (fmap (toFunction testTrans)),
        toFunctionTest "map'"      (IncSeq.map' testFun)
                                   (fmap testFun),
        toFunctionTest "concat"    IncSeq.concat
                                   (concatSeq :: Seq (Seq A) -> Seq A),
        toFunctionTest "singleton" IncSeq.singleton
                                   (Seq.singleton :: A -> Seq A),
        toFunctionTest "gate"      (IncSeq.gate testPrdTrans)
                                   (gateSeq (toFunction testPrdTrans)),
        toFunctionTest "gate'"     (IncSeq.gate' testPrdFun)
                                   (gateSeq testPrdFun),
        toFunctionTest "filter"    (IncSeq.filter testPrdTrans)
                                   (Seq.filter (toFunction testPrdTrans)),
        toFunctionTest "filter'"   (IncSeq.filter' testPrdFun)
                                   (Seq.filter testPrdFun),
        toFunctionTest "reverse"   IncSeq.reverse
                                   (Seq.reverse :: Seq A -> Seq A)
    ]

concatSeq :: Seq (Seq a) -> Seq a
concatSeq = asum

gateSeq :: (a -> Bool) -> a -> Seq a
gateSeq prd val | prd val   = Seq.singleton val
                | otherwise = Seq.empty
