{-# LANGUAGE UndecidableInstances #-}
module TestSuite (

    -- * Changes

    AtomicAChange (DoubleAndAdd),
    AtomicBChange (TripleAndAdd),

    -- * Test functions and transformations

    testTrans,
    testFun,
    testPrdTrans,
    testPrdFun,
    testCompare,

    -- * Test pattern

    transTest

) where

-- Prelude

import Prelude hiding (id, (.))

-- Control

import Control.Category
import Control.Applicative

-- Data

import           Data.Foldable (toList)
import           Data.MultiChange (MultiChange)
import qualified Data.MultiChange               as MultiChange
import           Data.Sequence (Seq)
import qualified Data.Sequence                  as Seq
import           Data.Incremental
import qualified Data.Incremental.Tuple         as Tuple
import qualified Data.Incremental.Sequence      as Seq

-- Test

import Test.QuickCheck
import Test.QuickCheck.Poly

-- Distribution

import Distribution.TestSuite
import Distribution.TestSuite.QuickCheck

-- * Test data generation

instance Arbitrary a => Arbitrary (Seq a) where

    arbitrary = fmap Seq.fromList arbitrary

    shrink seq = map Seq.fromList (shrink (toList seq))

-- * Changes

-- ** Common changes

instance Arbitrary a => Arbitrary (PrimitiveChange a) where

    arbitrary = frequency [(1, keepGen), (5, replaceGen)] where

        keepGen = return Keep

        replaceGen = fmap Replace arbitrary

    shrink Keep          = []
    shrink (Replace val) = Keep : map Replace (shrink val)

instance Arbitrary p => Arbitrary (MultiChange p) where

    arbitrary = fmap MultiChange.fromList arbitrary

    shrink change = map MultiChange.fromList (shrink (toList change))

-- ** Pair changes

deriving instance (Show (StdChange a), Show (StdChange b)) =>
                  Show (Tuple.AtomicChange a b)

instance (Arbitrary (StdChange a), Arbitrary (StdChange b)) =>
         Arbitrary (Tuple.AtomicChange a b) where

    arbitrary = oneof [firstGen, secondGen] where

        firstGen = fmap Tuple.First arbitrary

        secondGen = fmap Tuple.Second arbitrary

    shrink (Tuple.First change)  = map Tuple.First (shrink change)
    shrink (Tuple.Second change) = map Tuple.Second (shrink change)

-- ** Sequence changes

deriving instance (Show a, Show (StdChange a)) => Show (Seq.AtomicChange a)

instance (Arbitrary a, Arbitrary (StdChange a)) =>
         Arbitrary (Seq.AtomicChange a) where

    arbitrary = oneof [insertGen, deleteGen, shiftGen, changeAtGen] where

        insertGen = liftA2 Seq.Insert arbitrary arbitrary

        deleteGen = liftA2 Seq.Delete arbitrary arbitrary

        shiftGen = liftA3 Seq.Shift arbitrary arbitrary arbitrary

        changeAtGen = liftA2 Seq.ChangeAt arbitrary arbitrary

    shrink (Seq.Insert ix seq)
        = [Seq.Insert ix' seq'
              | (ix', seq') <- shrink (ix, seq)]
    shrink (Seq.Delete ix len)
        = [Seq.Delete ix' len'
              | (ix', len') <- shrink (ix, len)]
    shrink (Seq.Shift src len tgt)
        = [Seq.Shift src' len' tgt'
              | (src', len', tgt') <- shrink (src, len, tgt)]
    shrink (Seq.ChangeAt ix change)
        = [Seq.ChangeAt ix' change'
              | (ix', change') <- shrink (ix, change)]

-- ** Element changes

newtype AtomicAChange = DoubleAndAdd Integer deriving (Show, Arbitrary)

instance Change AtomicAChange where

    type Value AtomicAChange = A

    DoubleAndAdd diff $$ A integer = A (2 * integer + diff)

instance Changeable A where

    type StdChange A = MultiChange AtomicAChange

instance Ord A where

    compare (A integer1) (A integer2) = compare integer1 integer2

newtype AtomicBChange = TripleAndAdd Integer deriving (Show, Arbitrary)

instance Change AtomicBChange where

    type Value AtomicBChange = B

    TripleAndAdd diff $$ B integer = B (3 * integer + diff)

instance Changeable B where

    type StdChange B = MultiChange AtomicBChange

instance Changeable C

-- * Test functions and transformations

testTrans :: A ->> B
testTrans = MultiChange.map $ stateTrans init prop where

    init (A integer) = (B integer, integer)

    prop (DoubleAndAdd diff) state = (change', state') where

        change' = TripleAndAdd (diff - state)

        state' = 2 * state + diff

testFun :: C -> C
testFun = id

testPrdTrans :: A ->> Bool
testPrdTrans = MultiChange.composeMap $ stateTrans init prop where

    init (A integer) = (testPrd integer, integer)

    prop (DoubleAndAdd diff) state = (change', state') where

        change' = Replace (testPrd state')

        state' = 2 * state + diff

testPrdFun :: C -> Bool
testPrdFun = testPrd . unC

testPrd :: Integer -> Bool
testPrd = even

testCompare :: A -> A -> Ordering
testCompare (A integer1) (A integer2) = compare (integer1 `div` 3)
                                                (integer2 `div` 3)

-- * Test pattern

transTest :: (Show a, Arbitrary a, Changeable a,
              Show (StdChange a), Arbitrary (StdChange a),
              Eq b, Changeable b) =>
             String -> (a ->> b) -> (a -> b) -> Test
transTest name trans fun = testProperty name prop where

    prop valAndChanges = map fun (applyChanges valAndChanges) ==
                         applyChanges valAndChanges' where

        valAndChanges' = runTrans trans valAndChanges

applyChanges :: Change p => (Value p, [p]) -> [Value p]
applyChanges (val, changes) = scanl (flip ($$)) val changes
