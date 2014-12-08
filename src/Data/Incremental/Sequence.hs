module Data.Incremental.Sequence (

    -- * Changes

    AtomicChange (Insert, Delete, Shift),

    -- * Transformations

    map,
    concat,
    concatMap,
    filter,
    reverse

) where

-- Prelude

import Prelude hiding (id, (.), map, concat, concatMap, filter, reverse)

-- Category

import Control.Category

-- Data

import           Data.Monoid
import           Data.Foldable (asum, toList)
import           Data.FingerTree (FingerTree, Measured (measure))
import qualified Data.FingerTree as FingerTree
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.MultiChange (MultiChange)
import qualified Data.MultiChange as MultiChange
import           Data.Incremental

-- * Changes

data AtomicChange a = Insert !Int (Seq a)
                    | Delete !Int !Int
                    | Shift !Int !Int !Int
{-FIXME:
    Are these strictness annotations sensible? Should the sequence be strict?
-}

{-NOTE:
    Change application for sequences is total. It uses forms of saturation to
    achieve this. All the transformations must work correctly also in the
    saturation cases. At the time of writing, they do.
-}
instance Change (AtomicChange a) where

    type Value (AtomicChange a) = Seq a

    Insert ix seq' $$ seq = front <> seq' <> rear where

        (front,rear) = Seq.splitAt ix seq

    Delete ix len $$ seq = front <> rear where

        (front,rest) = Seq.splitAt ix seq

        (_,rear) = Seq.splitAt len rest

    Shift src len tgt $$ seq = Insert tgt mid $$ front <> rear where

        (front,rest) = Seq.splitAt src seq

        (mid,rear) = Seq.splitAt len rest

instance Changeable (Seq a) where

    type StdChange (Seq a) = MultiChange (AtomicChange a)

-- * Transformations

-- ** Mapping

map :: (a -> b) -> Seq a ->> Seq b
map fun = MultiChange.map $ statelessTrans (fmap fun) prop where

    prop (Insert ix seq)     = Insert ix (fmap fun seq)
    prop (Delete ix len)     = Delete ix len
    prop (Shift src len tgt) = Shift src len tgt

-- ** Concatenation

concatSeq :: Seq (Seq a) -> Seq a
concatSeq = asum

newtype ConcatStateElement = ConcatStateElement Int

data ConcatStateMeasure = ConcatStateMeasure {
                              sourceLength :: Int,
                              targetLength :: Int
                          }

instance Monoid ConcatStateMeasure where

    mempty = ConcatStateMeasure 0 0

    mappend (ConcatStateMeasure srcLen1 tgtLen1)
            (ConcatStateMeasure srcLen2 tgtLen2) = measure' where

        measure' = ConcatStateMeasure (srcLen1 + srcLen2) (tgtLen1 + tgtLen2)

instance Measured ConcatStateMeasure ConcatStateElement where

    measure (ConcatStateElement elLen) = ConcatStateMeasure 1 elLen

type ConcatState = FingerTree ConcatStateMeasure ConcatStateElement

seqToConcatState :: Seq (Seq a) -> ConcatState
seqToConcatState = FingerTree.fromList .
                   toList              .
                   fmap (ConcatStateElement . Seq.length)
                   
concat :: Seq (Seq a) ->> Seq a
concat = MultiChange.map $ pureTrans init prop where

    init seq = (concatSeq seq, seqToConcatState seq)

    prop (Insert ix seq) state = (change',state') where

        (ix',front,rear) = splitAndTranslate ix state

        change' = Insert ix' (concatSeq seq)

        state' = front <> seqToConcatState seq <> rear

    prop (Delete ix len) state = (change',state') where

        (ix',front,rest) = splitAndTranslate ix state

        (len',_,rear) = splitAndTranslate len rest

        change' = Delete ix' len'

        state' = front <> rear

    prop (Shift src len tgt) state = (change',state') where

        (src',front,rest) = splitAndTranslate src state

        (len',mid,rear) = splitAndTranslate len rest

        (tgt',front',rear') = splitAndTranslate tgt (front <> rear)

        change' = Shift src' len' tgt'

        state' = front' <> mid <> rear'

    splitAndTranslate :: Int -> ConcatState -> (Int,ConcatState,ConcatState)
    splitAndTranslate ix state = (targetLength (measure front),front,rear) where

        (front,rear) = FingerTree.split ((> ix) . sourceLength) state

-- ** Monadic structure

-- FIXME: Add return.

concatMap :: (a -> Seq b) -> Seq a ->> Seq b
concatMap fun = concat . map fun

-- ** Filtering

filter :: (a -> Bool) -> Seq a ->> Seq a
filter prd = concatMap (\ el -> if prd el then Seq.singleton el else Seq.empty)

-- ** Reversal

reverse :: Seq a ->> Seq a
reverse = undefined
-- FIXME: Implement this.
