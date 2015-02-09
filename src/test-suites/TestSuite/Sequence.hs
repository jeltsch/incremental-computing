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

-- * Tests

tests :: IO [Test]
tests = return [transTest "map"       (IncSeq.map testTrans)
                                      (fmap (toFunction testTrans)),
                transTest "map'"      (IncSeq.map' testFun)
                                      (fmap testFun),
                transTest "concat"    IncSeq.concat
                                      (seqConcat :: Seq (Seq A) -> Seq A),
                transTest "singleton" IncSeq.singleton
                                      (Seq.singleton :: A -> Seq A),
                transTest "gate"      (IncSeq.gate testPrdTrans)
                                      (seqGate (toFunction testPrdTrans)),
                transTest "gate'"     (IncSeq.gate' testPrdFun)
                                      (seqGate testPrdFun),
                transTest "filter"    (IncSeq.filter testPrdTrans)
                                      (Seq.filter (toFunction testPrdTrans)),
                transTest "filter'"   (IncSeq.filter' testPrdFun)
                                      (Seq.filter testPrdFun),
                transTest "reverse"   IncSeq.reverse
                                      (Seq.reverse :: Seq A -> Seq A),
                transTest "sort"      IncSeq.sort
                                      (Seq.sort :: Seq A -> Seq A),
                transTest "sortBy"    (IncSeq.sortBy testCompare)
                                      (Seq.sortBy testCompare)]
-- FIXME: Explain why we have no test for concatMap.

seqConcat :: Seq (Seq a) -> Seq a
seqConcat = asum

seqGate :: (a -> Bool) -> a -> Seq a
seqGate prd val | prd val   = Seq.singleton val
                | otherwise = Seq.empty
