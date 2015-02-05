module TestSuite (

    -- * Element changes

    AtomicAChange (DoubleAndAdd),
    AtomicBChange (TripleAndAdd),

    -- * Element conversions

    testTrans,
    testFun,
    testPrdTrans,
    testPrdFun,

    -- * Test patterns

    toFunctionTest

) where

-- Prelude

import Prelude hiding (id, (.))

-- Control

import Control.Category

-- Data

import           Data.Foldable (fold, toList)
import           Data.Incremental
import           Data.MultiChange (MultiChange)
import qualified Data.MultiChange               as MultiChange
import           Data.Sequence (Seq)
import qualified Data.Sequence                  as Seq

-- Test

import Test.QuickCheck
import Test.QuickCheck.Poly

-- Distribution

import Distribution.TestSuite
import Distribution.TestSuite.QuickCheck

-- * QuickCheck integration of sequences

instance Arbitrary a => Arbitrary (Seq a) where

    arbitrary = fmap Seq.fromList arbitrary

    shrink seq = map Seq.fromList (shrink (toList seq))

-- * Element changes

-- ** A

newtype AtomicAChange = DoubleAndAdd Integer deriving Show

instance Change AtomicAChange where

    type Value AtomicAChange = A

    DoubleAndAdd diff $$ A integer = A (2 * integer + diff)

instance Changeable A where

    type StdChange A = MultiChange AtomicAChange

-- ** B

newtype AtomicBChange = TripleAndAdd Integer deriving Show

instance Change AtomicBChange where

    type Value AtomicBChange = B

    TripleAndAdd diff $$ B integer = B (3 * integer + diff)

instance Changeable B where

    type StdChange B = MultiChange AtomicBChange

-- ** C

instance Changeable C

-- * Element conversions

testTrans :: A ->> B
testTrans = MultiChange.map $ stateTrans init prop where

    init (A integer) = (B integer, integer)

    prop (DoubleAndAdd diff) state = (change', state') where

        change' = TripleAndAdd (diff - state)

        state' = 2 * state + diff

testFun :: C -> C
testFun = id

testPrdTrans :: A ->> Bool
testPrdTrans = simpleTrans id fold .
               (MultiChange.map $ stateTrans init prop) where

    init (A integer) = (testPrd integer, integer)

    prop (DoubleAndAdd diff) state = (change', state') where

        change' = Replace (testPrd state')

        state' = 2 * state + diff

testPrdFun :: C -> Bool
testPrdFun = testPrd . unC

testPrd :: Integer -> Bool
testPrd = even

-- * Test patterns

toFunctionTest :: (Show a, Arbitrary a, Changeable a, Eq b, Changeable b) =>
                  String -> (a ->> b) -> (a -> b) -> Test
toFunctionTest name trans fun = testProperty name prop where

    prop val = toFunction trans val == fun val
