module TestSuite () where

-- Data

import           Data.Foldable (toList)
import           Data.Incremental
import           Data.Sequence (Seq)
import qualified Data.Sequence          as Seq

-- Test

import Test.QuickCheck
import Test.QuickCheck.Poly

instance Arbitrary a => Arbitrary (Seq a) where

    arbitrary = fmap Seq.fromList arbitrary

    shrink seq = map Seq.fromList (shrink (toList seq))

instance Changeable A

instance Changeable B

instance Changeable C
