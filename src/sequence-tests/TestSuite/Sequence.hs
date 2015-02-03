module TestSuite.Sequence (

    tests

) where

-- Data

import           Data.Foldable (toList)
import           Data.Incremental
import           Data.Sequence (Seq)
import qualified Data.Sequence             as Seq
import qualified Data.Incremental.Sequence as IncrementalSeq

-- Test

import Test.QuickCheck
import Test.QuickCheck.Poly

-- Distribution

import Distribution.TestSuite
import Distribution.TestSuite.QuickCheck

-- * List of tests

tests :: IO [Test]
tests = return [
            toFunctionTest
        ]

-- * Individual tests

toFunctionTest :: Test
toFunctionTest = testGroup "toFunction" [reverseTest] where

    reverseTest :: Test
    reverseTest = testProperty "toFunction on reverse" prop where

        prop :: Seq A -> Bool
        prop seq = toFunction IncrementalSeq.reverse seq == Seq.reverse seq

-- * Utilities

instance Arbitrary a => Arbitrary (Seq a) where

    arbitrary = fmap Seq.fromList arbitrary

    shrink seq = map Seq.fromList (shrink (toList seq))

instance Changeable A

instance Changeable B

instance Changeable C
