module TestSuite (

    tests

) where

-- Distribution

import Distribution.TestSuite
import Distribution.TestSuite.QuickCheck

-- Test

import Test.QuickCheck
import Test.QuickCheck.Poly

-- * List of tests

tests :: IO [Test]
tests = return [
            example
        ]

-- * Individual tests

example :: Test
example = testProperty "Example property" prop where

    prop :: [A] -> Bool
    prop list = reverse (reverse list) == list
