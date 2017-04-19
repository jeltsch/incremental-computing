module Data.Sequence.Incremental (

    -- * Type

    type Seq,

    -- * Operations

    type SeqCoreOperations (ElemCoreOps, ElemPacket, focus),
    type CoreOps (CoreOps, empty, singleton, onSlice, onElem),

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
import           Data.Sequence (Seq, (<|), (><), ViewL ((:<)))
import qualified Data.Sequence as Seq

-- * Type

instance Data a => Data (Seq a) where

    type CoreOperations (Seq a) o = (
             SeqCoreOperations o,
             CoreOperations a (ElemCoreOps o)
         )

    type StdCoreOps (Seq a) = CoreOps (StdCoreOps a) a

    stdCoreOps = CoreOps {
        empty = Seq.empty,
        singleton = \ newElem -> Seq.singleton (newElem stdOps),
        onSlice = \ sliceIdx sliceLen procSlice -> do
            seq <- get
            let (prefix, rest) = Seq.splitAt sliceIdx seq
            let (slice, suffix) = Seq.splitAt sliceLen rest
            let (result, slice') = runState (procSlice stdOps) slice
            put (prefix >< slice' >< suffix)
            return result,
        onElem = \ elemIdx procElem -> do
            seq <- get
            let (prefix, rest) = Seq.splitAt elemIdx seq
            let (elem :< suffix) = Seq.viewl rest
            let (result, elem') = runState (procElem stdOps) elem
            put (prefix >< elem' <| suffix)
            return result
    }

class SeqCoreOperations o where

    type ElemCoreOps o :: * -> * -> *

    type ElemPacket o :: *

    focus :: o _seq seq -> CoreOps (ElemCoreOps o) (ElemPacket o) _seq seq

instance SeqCoreOperations (CoreOps elemCoreOps _elem) where

    type ElemCoreOps (CoreOps elemCoreOps _) = elemCoreOps

    type ElemPacket (CoreOps _ _elem) = _elem

    focus = id

-- * Operations

data CoreOps elemCoreOps _elem _seq seq = CoreOps {
    empty     :: seq,
    singleton :: (forall elem . Ops elemCoreOps _elem elem -> elem)
              -> seq,
    onSlice   :: forall r .
                 Int
              -> Int
              -> (forall seq' . Ops (CoreOps elemCoreOps _elem) _seq seq' ->
                                State seq' r)
              -> State seq r,
    onElem    :: forall r .
                 Int
              -> (forall elem . Ops elemCoreOps _elem elem ->
                                State elem r)
              -> State seq r
}

-- * Transformations

reverse :: Data a => Seq a ->> Seq a
reverse = Trans $ \ gen -> fmap fst . gen . opsConv where

    opsConv :: SeqCoreOperations o
            => Ops o _seq seq
            -> Ops (CoreOps (ElemCoreOps o) (ElemPacket o))
                   (_seq, Int)
                   (seq, Int)
    opsConv (Ops { .. }) = Ops {
        pack    = first pack,
        unpack  = first unpack,
        coreOps = coreOpsConv coreOps
    }

    coreOpsConv :: SeqCoreOperations o
                => o _seq seq
                -> CoreOps (ElemCoreOps o) (ElemPacket o) (_seq, Int) (seq, Int)
    coreOpsConv (focus -> CoreOps { .. }) = CoreOps {
        empty = (empty, 0),
        singleton = \ newElem -> (singleton newElem, 1),
        onSlice = \ sliceIdx sliceLen procSlice -> toPairState $ \ len -> do
            let revSliceIdx = len - sliceIdx - sliceLen
            let revSliceLen = sliceLen
            (result, sliceLen') <- onSlice revSliceIdx revSliceLen $
                                   \ revSliceOps -> do
                                       fromPairState
                                           (procSlice (opsConv revSliceOps))
                                           sliceLen
            return (result, len - sliceLen + sliceLen'),
        onElem = \ elemIdx procElem -> toPairState $ \ len -> do
            let revElemIdx = len - elemIdx - 1
            result <- onElem revElemIdx procElem
            return (result, len)
    }

fromPairState :: State (s, e) a -> e -> State s (a, e)
fromPairState comp ext = state fun where

    fun state = ((result, ext'), state') where

        (result, (state', ext')) = runState comp (state, ext)

toPairState :: (e -> State s (a, e)) -> State (s, e) a
toPairState compFromExt = state fun where

    fun (state, ext) = (result, (state', ext')) where

        ((result, ext'), state') = runState (compFromExt ext) state
