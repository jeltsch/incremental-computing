module Data.Sequence.Incremental (

    -- * Type

    type Seq,

    -- * Operations

    type SeqOps (SeqOps, empty, singleton, onSlice, onElement),

    -- * Transformations

    reverse

) where

-- Prelude

import Prelude hiding (reverse)

-- Control

import Control.Monad.Trans.State
import Control.Arrow

-- Data

import           Data.Incremental
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- * Type

instance Data a => Data (Seq a) where

    newtype Specific (Seq a) u = SeqSpecific (
                forall elemOps _elem . (Operations elemOps, Dat elemOps ~ a) =>
                u (SeqOps elemOps _elem)
            )

-- * Operations

data SeqOps elemOps _elem _seq seq = SeqOps {
    empty     :: seq,
    singleton :: (forall elem . AllOps elemOps _elem elem -> elem)
              -> seq,
    onSlice   :: forall r .
                 Int
              -> Int
              -> (forall seq' . AllOps (SeqOps elemOps _elem) _seq seq' ->
                                State seq' r)
              -> State seq r,
    onElement :: forall r .
                 Int
              -> (forall elem . AllOps elemOps _elem elem ->
                                State elem r)
              -> State seq r
}

instance Operations elemOps => Operations (SeqOps elemOps _elem) where

    type Dat (SeqOps elemOps _) = Seq (Dat elemOps)

    generalize (SeqSpecific ops) = ops

-- * Transformations

reverse :: Data a => Seq a ->> Seq a
reverse = Trans $ \ gen -> unAllOpsCont      $
                           generalize        $
                           SeqSpecific       $
                           AllOpsCont        $
                           gen  . allOpsConv where

    allOpsConv :: AllOps (SeqOps elemOps _elem) _seq seq
               -> AllOps (SeqOps elemOps _elem) (_seq, Int) (seq, Int)
    allOpsConv (AllOps { .. }) = AllOps {
        pack   = first pack,
        unpack = first unpack,
        ops    = seqOpsConv ops
    }

    seqOpsConv :: SeqOps elemOps _elem _seq seq
               -> SeqOps elemOps _elem (_seq, Int) (seq, Int)
    seqOpsConv (SeqOps { .. }) = SeqOps {
        empty = (empty, 0),
        singleton = \ newElem -> (singleton newElem, 1),
        onSlice = \ sliceIdx sliceLen procSlice -> toPairState $ \ len -> do
            let revSliceIdx = len - sliceIdx - sliceLen
            let revSliceLen = sliceLen
            (result, sliceLen') <- onSlice revSliceIdx revSliceLen $
                                   \ revSliceOps -> do
                                       fromPairState
                                           (procSlice (allOpsConv revSliceOps))
                                           sliceLen
            return (result, len - sliceLen + sliceLen')
    }

fromPairState :: State (s, e) a -> e -> State s (a, e)
fromPairState comp ext = state fun where

    fun state = ((result, ext'), state') where

        (result, (state', ext')) = runState comp (state, ext)

toPairState :: (e -> State s (a, e)) -> State (s, e) a
toPairState compFromExt = state fun where

    fun (state, ext) = (result, (state', ext')) where

        ((result, ext'), state') = runState (compFromExt ext) state
