module Data.Sequence.Incremental (

    -- * Core operations

    type CoreOps (CoreOps, empty, singleton, onSlice, onElem),

    -- * Transformations

    concat,
    reverse

) where

-- Prelude

import Prelude hiding (concat, reverse)

-- Control

import Control.Monad.Trans.State
import Control.Arrow

-- Data

import           Data.Kind (Type)
import           Data.Type.Equality
import           Data.Incremental
import           Data.FingerTree (FingerTree, Measured (measure))
import qualified Data.FingerTree as FingerTree
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- * Data

instance Data a => Data (Seq a) where

    data CanonicalCoreOps (Seq a) o where

        CanonicalCoreOps :: CoreOperations elemCoreOps
                         => CanonicalCoreOps (Seq (DataOf elemCoreOps))
                                             (CoreOps elemCoreOps)

    coreOpsEqFromCan CanonicalCoreOps CanonicalCoreOps = lift coreOpsEq where

        lift :: elemCoreOps1 :~~: elemCoreOps2
             -> CoreOps elemCoreOps1 :~~: CoreOps elemCoreOps2
        lift HRefl = HRefl

-- * Operations

data Internal j = Internal j Type

data CoreOps (elemCoreOps :: j -> Type -> Type -> Type)
             (seqInternal :: Internal j)
             (seqPacket   :: Type)
             (seq         :: Type)
    where

    CoreOps :: {

        empty :: seq,

        singleton :: ArgMaker elemCoreOps elemInternal elemPacket -> seq,

        onSlice :: Int
                -> Int
                -> LensOp (CoreOps elemCoreOps)
                          ('Internal elemInternal elemPacket)
                          seqPacket
                          seq,

        onElem :: Int -> LensOp elemCoreOps elemInternal elemPacket seq

    } -> CoreOps elemCoreOps ('Internal elemInternal elemPacket) seqPacket seq

instance CoreOperations elemCoreOps =>
         CoreOperations (CoreOps elemCoreOps) where

    type DataOf (CoreOps elemCoreOps) = Seq (DataOf elemCoreOps)

    canonicalCoreOps = CanonicalCoreOps

    type StdInternal (CoreOps elemCoreOps)
        = 'Internal (StdInternal elemCoreOps) (DataOf elemCoreOps)

    stdCoreOps = CoreOps {

        empty = Seq.empty,

        singleton = \ newElem -> Seq.singleton (newElem stdOps),

        onSlice = \ sliceIx sliceLen procSlice -> do
            seq <- get
            let (prefix, rest) = Seq.splitAt sliceIx seq
            let (slice, suffix) = Seq.splitAt sliceLen rest
            let (result, slice') = runState (procSlice stdOps) slice
            put (prefix Seq.>< slice' Seq.>< suffix)
            return result,

        onElem = \ elemIx procElem -> do
            seq <- get
            let (prefix, rest) = Seq.splitAt elemIx seq
            let (elem Seq.:< suffix) = Seq.viewl rest
            let (result, elem') = runState (procElem stdOps) elem
            put (prefix Seq.>< elem' Seq.<| suffix)
            return result

    }

-- * Transformations

-- FIXME: Check strictness of tuples.

-- ** Concatenation

concat :: Seq (Seq a) ->> Seq a
concat = Trans $ \ (Generator genFun) -> conv genFun where

    conv :: forall a o f . (Functor f, CoreOperations o, DataOf o ~ Seq (Seq a))
         => (forall i p e . Ops o i p e -> f e)
         -> Generator (Seq a) f
    conv = case canonicalCoreOps @_ @o of CanonicalCoreOps -> conv'

    conv' :: forall a o f . (Functor f, CoreOperations o, DataOf o ~ Seq a)
          => (forall i p e . Ops (CoreOps o) i p e -> f e)
          -> Generator (Seq a) f
    conv' = case canonicalCoreOps @_ @o of CanonicalCoreOps -> conv''

    conv'' :: forall a o f . (Functor f, CoreOperations o, DataOf o ~ a)
           => (forall i p e . Ops (CoreOps (CoreOps o)) i p e -> f e)
           -> Generator (Seq a) f
    conv'' = infoTransCore (. opsConv)

    opsConv :: Ops (CoreOps elemCoreOps)
                   seqInternal
                   seqPacket
                   seq
            -> Ops (CoreOps (CoreOps elemCoreOps))
                   ('Internal seqInternal (seqPacket, Int))
                   (seqPacket, ConcatInfo)
                   (seq, ConcatInfo)
    opsConv = dynInfoOpsConv $
              \ ops@(Ops { coreOps = CoreOps { .. } }) -> CoreOps {

        empty = (empty, FingerTree.empty),

        singleton = \ newElem -> second
                                     (FingerTree.singleton . ConcatInfoElem)
                                     (newElem (lengthOps ops)),

        onSlice = \ sliceIx sliceLen procSlice -> toPairState $ \ info -> do
            let (infoPrefix, infoRest) = splitConcatInfoAt sliceIx
                                                           info
            let (infoSlice, infoSuffix) = splitConcatInfoAt sliceLen
                                                            infoRest
            let flatSliceIx = targetLength (measure infoPrefix)
            let flatSliceLen = targetLength (measure infoSlice)
            (result, infoSlice') <- onSlice flatSliceIx flatSliceLen $
                                    \ flatSliceOps -> do
                fromPairState (procSlice (opsConv flatSliceOps)) infoSlice
            let info' = infoPrefix FingerTree.><
                        infoSlice' FingerTree.><
                        infoSuffix
            return (result, info'),

        onElem = \ elemIx procElem -> toPairState $ \ info -> do
            let (infoPrefix, infoRest) = splitConcatInfoAt elemIx info
            let infoElem FingerTree.:< infoSuffix = FingerTree.viewl $
                                                    infoRest
            let flatSliceIx = targetLength (measure infoPrefix)
            let ConcatInfoElem flatSliceLen = infoElem
            (result, flatSliceLen') <- onSlice flatSliceIx flatSliceLen $
                                       \ flatSliceOps -> do
                fromPairState (procElem (lengthOps flatSliceOps))
                              flatSliceLen
            let infoElem' = ConcatInfoElem flatSliceLen'
            let info' = infoPrefix FingerTree.><
                        infoElem'  FingerTree.<|
                        infoSuffix
            return (result, info')

    }

lengthOps :: Ops (CoreOps elemCoreOps)
                 ('Internal elemInternal elemPacket)
                 seqPacket
                 seq
          -> Ops (CoreOps elemCoreOps)
                 ('Internal elemInternal elemPacket)
                 (seqPacket, Int)
                 (seq, Int)
lengthOps (Ops { coreOps = CoreOps { .. }, .. }) = Ops {
    pack = first pack,
    unpack = first unpack,
    coreOps = CoreOps {

        empty = (empty, 0),

        singleton = \ newElem -> (singleton newElem, 1),

        onSlice = \ sliceIx sliceLen procSlice -> toPairState $ \ len -> do
            (result, sliceLen') <- onSlice sliceIx sliceLen $ \ sliceOps -> do
               fromPairState (procSlice (lengthOps sliceOps)) sliceLen
            return (result, len - sliceLen + sliceLen'),

        onElem = \ elemIx procElem -> toPairState $ \ len -> do
            result <- onElem elemIx procElem
            return (result, len)

    }
}

type ConcatInfo = FingerTree ConcatInfoMeasure ConcatInfoElem

newtype ConcatInfoElem = ConcatInfoElem Int

data ConcatInfoMeasure = ConcatInfoMeasure {
                             sourceLength :: !Int,
                             targetLength :: !Int
                         }

instance Monoid ConcatInfoMeasure where

    mempty = ConcatInfoMeasure 0 0

    mappend (ConcatInfoMeasure srcLen1 tgtLen1)
            (ConcatInfoMeasure srcLen2 tgtLen2) = measure' where

        measure' = ConcatInfoMeasure (srcLen1 + srcLen2) (tgtLen1 + tgtLen2)

instance Measured ConcatInfoMeasure ConcatInfoElem where

    measure (ConcatInfoElem elemLen) = ConcatInfoMeasure 1 elemLen

splitConcatInfoAt :: Int -> ConcatInfo -> (ConcatInfo, ConcatInfo)
splitConcatInfoAt ix = FingerTree.split ((> ix) . sourceLength)

-- ** Reversal

-- FIXME: Use lengthOps.

reverse :: Seq a ->> Seq a
reverse = Trans $ \ (Generator genFun) -> conv genFun where

    conv :: forall a o f . (Functor f, CoreOperations o, DataOf o ~ Seq a)
         => (forall i p e . Ops o i p e -> f e)
         -> Generator (Seq a) f
    conv = case canonicalCoreOps @_ @o of CanonicalCoreOps -> conv'

    conv' :: forall a o f . (Functor f, CoreOperations o, DataOf o ~ a)
          => (forall i p e . Ops (CoreOps o) i p e -> f e)
          -> Generator (Seq a) f
    conv' = infoTransCore (. opsConv)

    opsConv :: Ops (CoreOps elemCoreOps)
                   seqInternal
                   seqPacket
                   seq
            -> Ops (CoreOps elemCoreOps)
                   seqInternal
                   (seqPacket, Int)
                   (seq, Int)
    opsConv = dynInfoOpsConv coreOpsConv

    coreOpsConv :: Ops (CoreOps elemCoreOps)
                       seqInternal
                       seqPacket
                       seq
                -> CoreOps elemCoreOps
                           seqInternal
                           (seqPacket, Int)
                           (seq, Int)
    coreOpsConv (Ops { coreOps = CoreOps { .. } }) = CoreOps {

        empty = (empty, 0),

        singleton = \ newElem -> (singleton newElem, 1),

        onSlice = \ sliceIx sliceLen procSlice -> toPairState $ \ len -> do
            let revSliceIx = len - sliceIx - sliceLen
            let revSliceLen = sliceLen
            (result, sliceLen') <- onSlice revSliceIx revSliceLen $
                                   \ revSliceOps -> do
                                       fromPairState
                                           (procSlice (opsConv revSliceOps))
                                           sliceLen
            return (result, len - sliceLen + sliceLen'),

        onElem = \ elemIx procElem -> toPairState $ \ len -> do
            let revElemIx = len - elemIx - 1
            result <- onElem revElemIx procElem
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
