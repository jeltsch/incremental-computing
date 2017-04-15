module Data.Sequence.Incremental (

    -- * Type

    type Seq,
    type Specific (SeqSpecific),

    -- * Operations

    type SeqOps (SeqOps, empty, singleton, onSlice, onElem),

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
                forall elemOps _elem . (CoreOps elemOps, DataOf elemOps ~ a) =>
                u (SeqOps elemOps _elem)
            )

-- * Operations

data SeqOps elemOps _elem _seq seq = SeqOps {
    empty     :: seq,
    singleton :: (forall elem . Ops elemOps _elem elem -> elem)
              -> seq,
    onSlice   :: forall r .
                 Int
              -> Int
              -> (forall seq' . Ops (SeqOps elemOps _elem) _seq seq' ->
                                State seq' r)
              -> State seq r,
    onElem    :: forall r .
                 Int
              -> (forall elem . Ops elemOps _elem elem ->
                                State elem r)
              -> State seq r
}

instance CoreOps elemOps => CoreOps (SeqOps elemOps _elem) where

    type DataOf (SeqOps elemOps _) = Seq (DataOf elemOps)

    generalize (SeqSpecific val) = val

-- * Transformations

reverse :: Data a => Seq a ->> Seq a
reverse = Trans $ \ gen -> unOpsCont         $
                           generalize        $
                           SeqSpecific       $
                           OpsCont           $
                           gen  . allOpsConv where

    allOpsConv :: Ops (SeqOps elemOps _elem) _seq seq
               -> Ops (SeqOps elemOps _elem) (_seq, Int) (seq, Int)
    allOpsConv (Ops { .. }) = Ops {
        pack    = first pack,
        unpack  = first unpack,
        coreOps = seqOpsConv coreOps
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
