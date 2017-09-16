module Data.Sequence.Incremental (

    -- * Transformations

    concat,
    reverse,

    -- * Core operations

    type CoreOps (CoreOps, empty, singleton, onSlice, onElem)

) where

-- Prelude

import Prelude hiding (concat, reverse)

-- Control

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Arrow

-- Data

import           Data.Kind (Type)
import           Data.Type.Equality
import           Data.Tuple
import           Data.FingerTree (FingerTree, Measured (measure))
import qualified Data.FingerTree as FingerTree
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Incremental

-- * Transformations

-- FIXME: Check strictness of tuples.

-- ** Concatenation

concat :: Seq (Seq a) ->> Seq a
concat = Trans $ \ (Generator genFun) -> preTrans0 genFun where

    preTrans0 :: forall a o f .
                 (Functor f, CoreOperations o, DataOf o ~ Seq (Seq a))
              => (forall i p e . Ops o i p e -> f e)
              -> Generator (Seq a) f
    preTrans0 = case canonicalCoreOps @_ @o of CanonicalCoreOps -> preTrans1

    preTrans1 :: forall a o f .
                 (Functor f, CoreOperations o, DataOf o ~ Seq a)
              => (forall i p e . Ops (CoreOps o) i p e -> f e)
              -> Generator (Seq a) f
    preTrans1 = case canonicalCoreOps @_ @o of CanonicalCoreOps -> preTrans2

    preTrans2 :: forall a o f .
                 (Functor f, CoreOperations o, DataOf o ~ a)
              => (forall i p e . Ops (CoreOps (CoreOps o)) i p e -> f e)
              -> Generator (Seq a) f
    preTrans2 = infoPreTrans $ InfoTransCore opsConv

    opsConv :: Ops (CoreOps elemCoreOps)
                   seqInternal
                   seqPacket
                   seq
            -> Ops (CoreOps (CoreOps elemCoreOps))
                   ('Internal seqInternal (seqPacket, Int))
                   (seqPacket, ConcatInfo)
                   (seq, ConcatInfo)
    opsConv ops@(Ops { coreOps = CoreOps { .. }, .. })
        = dynamicInfoOps pack unpack $
          CoreOps empty' singleton' onSlice' onElem'

        where

        empty' = (empty, FingerTree.empty)

        singleton' = infoConstructorMap infoFromLength $
                     wholeConstructor (lengthOps ops)

            where

            infoFromLength :: Int -> ConcatInfo
            infoFromLength = FingerTree.singleton . ConcatInfoElem

        onSlice' sliceIx sliceLen = jointInfoEditor crop splice $
                                    withInputInfo $ \ (infoPrefix, _) ->
                                    flatInfoEditorLift $
                                    withInputInfo $ \ infoSlice ->
                                    deepInfoEditorLift opsConv $
                                    onSlice (infoTargetLength infoPrefix)
                                            (infoTargetLength infoSlice)

            where

            crop :: ConcatInfo -> (ConcatInfo, (ConcatInfo, ConcatInfo))
            crop info = (infoSlice, (infoPrefix, infoSuffix))

                where

                (infoPrefix, infoRest) = splitConcatInfoAt sliceIx info

                (infoSlice, infoSuffix) = splitConcatInfoAt sliceLen infoRest

            splice :: (ConcatInfo, (ConcatInfo, ConcatInfo)) -> ConcatInfo
            splice (infoSlice, (infoPrefix, infoSuffix))
                = infoPrefix FingerTree.>< infoSlice FingerTree.>< infoSuffix

        onElem' elemIx = jointInfoEditor crop splice $
                         withInputInfo $ \ (infoPrefix, _) ->
                         flatInfoEditorLift $
                         withInputInfo $ \ elemLen ->
                         deepInfoEditorLift lengthOps $
                         onSlice (infoTargetLength infoPrefix) elemLen

            where

            crop :: ConcatInfo -> (Int, (ConcatInfo, ConcatInfo))
            crop info = (elemLen, (infoPrefix, infoSuffix))

                where

                (infoPrefix, infoRest) = splitConcatInfoAt elemIx info

                infoElem FingerTree.:< infoSuffix = FingerTree.viewl infoRest

                ConcatInfoElem elemLen = infoElem

            splice :: (Int, (ConcatInfo, ConcatInfo)) -> ConcatInfo
            splice (elemLen, (infoPrefix, infoSuffix))
                = infoPrefix             FingerTree.><
                  ConcatInfoElem elemLen FingerTree.<|
                  infoSuffix

        infoTargetLength :: ConcatInfo -> Int
        infoTargetLength = targetLength . measure

lengthOps :: Ops (CoreOps elemCoreOps)
                 ('Internal elemInternal elemPacket)
                 seqPacket
                 seq
          -> Ops (CoreOps elemCoreOps)
                 ('Internal elemInternal elemPacket)
                 (seqPacket, Int)
                 (seq, Int)
lengthOps (Ops { coreOps = CoreOps { .. }, .. })
    = dynamicInfoOps pack unpack $
      CoreOps empty' singleton' onSlice' onElem'

    where

    empty' = (empty, 0)

    singleton' = flatInfoConstructorLift 1 singleton

    onSlice' sliceIx sliceLen = jointInfoEditor crop splice $
                                withInputInfo $ \ lenDiff ->
                                flatInfoEditorLift $
                                deepInfoEditorLift lengthOps $
                                onSlice sliceIx sliceLen

        where

        crop :: Int -> (Int, Int)
        crop len = (sliceLen, len - sliceLen)

        splice :: (Int, Int) -> Int
        splice (sliceLen, lenDiff) = sliceLen + lenDiff

    onElem' elemIx = withInputInfo $ \ len ->
                     flatInfoEditorLift $
                     onElem elemIx

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
reverse = Trans $ \ (Generator genFun) -> preTrans0 genFun where

    preTrans0 :: forall a o f .
                 (Functor f, CoreOperations o, DataOf o ~ Seq a)
              => (forall i p e . Ops o i p e -> f e)
              -> Generator (Seq a) f
    preTrans0 = case canonicalCoreOps @_ @o of CanonicalCoreOps -> preTrans1

    preTrans1 :: forall a o f .
                 (Functor f, CoreOperations o, DataOf o ~ a)
              => (forall i p e . Ops (CoreOps o) i p e -> f e)
              -> Generator (Seq a) f
    preTrans1 = infoPreTrans $ InfoTransCore opsConv

    opsConv :: Ops (CoreOps elemCoreOps)
                   seqInternal
                   seqPacket
                   seq
            -> Ops (CoreOps elemCoreOps)
                   seqInternal
                   (seqPacket, Int)
                   (seq, Int)
    opsConv (Ops { coreOps = CoreOps { .. }, .. })
        = dynamicInfoOps pack unpack $
          CoreOps empty' singleton' onSlice' onElem'

        where

        empty' = (empty, 0)

        singleton' = flatInfoConstructorLift 1 singleton

        onSlice' sliceIx sliceLen = jointInfoEditor crop splice $
                                    withInputInfo $ \ lenDiff ->
                                    flatInfoEditorLift $
                                    deepInfoEditorLift opsConv $
                                    onSlice (lenDiff - sliceIx) sliceLen

            where

            crop :: Int -> (Int, Int)
            crop len = (sliceLen, len - sliceLen)

            splice :: (Int, Int) -> Int
            splice (sliceLen, lenDiff) = sliceLen + lenDiff

        onElem' elemIx = withInputInfo $ \ len ->
                         flatInfoEditorLift $
                         onElem (pred len - elemIx)

fromPairState :: Functor f => StateT (s, e) f a -> e -> StateT s f (a, e)
fromPairState = runStateT . stateTFlip . stateTCurry

toPairState :: Functor f => (e -> StateT s f (a, e)) -> StateT (s, e) f a
toPairState = stateTUncurry . stateTFlip . StateT

{-FIXME:
    Remove the following duplicate of code from Data.Incremental once
    fromPairState and toPairState are removed.
-}

stateTCurry :: Functor f
            => StateT (s1, s2) f a
            -> StateT s1 (StateT s2 f) a
stateTCurry comp = StateT $ \ state1 -> StateT $ \ state2 ->
                   leftAssoc <$> comp `runStateT` (state1, state2)

stateTUncurry :: Functor f
              => StateT s1 (StateT s2 f) a
              -> StateT (s1, s2) f a
stateTUncurry comp = StateT $ \ (state1, state2) ->
                     rightAssoc <$> (comp `runStateT` state1) `runStateT` state2

stateTFlip :: Functor f
           => StateT s1 (StateT s2 f) a
           -> StateT s2 (StateT s1 f) a
stateTFlip comp = StateT $ \ state2 ->
                  StateT $ \ state1 ->
                  leftAssoc . second swap . rightAssoc <$>
                  (comp `runStateT` state1) `runStateT` state2

leftAssoc :: (a, (b, c)) -> ((a, b), c)
leftAssoc (val1, (val2, val3)) = ((val1, val2), val3)

rightAssoc :: ((a, b), c) -> (a, (b, c))
rightAssoc ((val1, val2), val3) = (val1, (val2, val3))

-- * Operations

data Internal j = Internal j Type

type instance UnitInternal = 'Internal UnitInternal ()

type instance ZipInternals ('Internal elemInternal1 elemPacket1)
                           ('Internal elemInternal2 elemPacket2)
    = 'Internal (ZipInternals elemInternal1 elemInternal2)
                (elemPacket1, elemPacket2)

data CoreOps (elemCoreOps :: j -> Type -> Type -> Type)
             (seqInternal :: Internal j)
             (seqPacket   :: Type)
             (seq         :: Type)
    where

    CoreOps :: {

        empty :: seq,

        singleton :: Constructor elemCoreOps elemInternal elemPacket seq,

        onSlice :: Int
                -> Int
                -> Editor (CoreOps elemCoreOps)
                          ('Internal elemInternal elemPacket)
                          seqPacket
                          seq,

        onElem :: Int
               -> Editor elemCoreOps elemInternal elemPacket seq

    } -> CoreOps elemCoreOps ('Internal elemInternal elemPacket) seqPacket seq

instance CoreOperations elemCoreOps =>
         CoreOperations (CoreOps elemCoreOps) where

    type DataOf (CoreOps elemCoreOps) = Seq (DataOf elemCoreOps)

    canonicalCoreOps = CanonicalCoreOps

    unitCoreOps = CoreOps {
        empty     = (),
        singleton = unitConstructor,
        onSlice   = (const . const) unitEditor,
        onElem    = const unitEditor
    }

    zipCoreOps (CoreOps empty1 singleton1 onSlice1 onElem1)
               (CoreOps empty2 singleton2 onSlice2 onElem2) = CoreOps {
        empty     = (empty1, empty2),
        singleton = zipConstructors singleton1 singleton2,
        onSlice   = (liftA2 . liftA2) zipEditors onSlice1 onSlice2,
        onElem    = liftA2 zipEditors onElem1 onElem2
    }

    type StdInternal (CoreOps elemCoreOps)
        = 'Internal (StdInternal elemCoreOps) (DataOf elemCoreOps)

    stdCoreOps = CoreOps {

        empty = Seq.empty,

        singleton = Constructor $
                    \ newElem -> Seq.singleton <$> newElem stdOps,

        onSlice = \ sliceIx sliceLen -> Editor $ \ procSlice -> do
            seq <- get
            let (prefix, rest) = Seq.splitAt sliceIx seq
            let (slice, suffix) = Seq.splitAt sliceLen rest
            (result, slice') <- lift $ runStateT (procSlice stdOps) slice
            put (prefix Seq.>< slice' Seq.>< suffix)
            return result,

        onElem = \ elemIx -> Editor $ \ procElem -> do
            seq <- get
            let (prefix, rest) = Seq.splitAt elemIx seq
            let (elem Seq.:< suffix) = Seq.viewl rest
            (result, elem') <- lift $ runStateT (procElem stdOps) elem
            put (prefix Seq.>< elem' Seq.<| suffix)
            return result

    }

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
