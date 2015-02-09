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
tests = return [transTest "singleton" IncSeq.singleton
                                      (Seq.singleton :: A -> Seq A),
                transTest "fromPair"  IncSeq.fromPair
                                      (seqFromPair :: (A, A) -> Seq A),
                transTest "cat"       IncSeq.cat
                                      (seqCat :: (Seq A, Seq A) -> Seq A),
                transTest "null"      IncSeq.null
                                      (Seq.null :: Seq A -> Bool),
                transTest "length"    IncSeq.length
                                      (Seq.length :: Seq A -> Int),
                transTest "map"       (IncSeq.map testTrans)
                                      (fmap (toFunction testTrans)),
                transTest "map'"      (IncSeq.map' testFun)
                                      (fmap testFun),
                transTest "concat"    IncSeq.concat
                                      (seqConcat :: Seq (Seq A) -> Seq A),
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

seqFromPair :: (a, a) -> Seq a
seqFromPair (val1, val2) = Seq.fromList [val1, val2]

seqCat :: (Seq a, Seq a) -> Seq a
seqCat = uncurry (Seq.><)

seqConcat :: Seq (Seq a) -> Seq a
seqConcat = asum

seqGate :: (a -> Bool) -> a -> Seq a
seqGate prd val | prd val   = Seq.singleton val
                | otherwise = Seq.empty
