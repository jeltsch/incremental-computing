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

-- Data

import           Data.Kind (Type)
import           Data.Type.Equality
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
                                    shallowEditorLift $
                                    withInputInfo $ \ infoSlice ->
                                    deepEditorLift opsConv $
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
                         shallowEditorLift $
                         withInputInfo $ \ elemLen ->
                         deepEditorLift lengthOps $
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

    singleton' = shallowConstructorLift 1 singleton

    onSlice' sliceIx sliceLen = jointInfoEditor crop splice $
                                withInputInfo $ \ lenDiff ->
                                shallowEditorLift $
                                deepEditorLift lengthOps $
                                onSlice sliceIx sliceLen

        where

        crop :: Int -> (Int, Int)
        crop len = (sliceLen, len - sliceLen)

        splice :: (Int, Int) -> Int
        splice (sliceLen, lenDiff) = sliceLen + lenDiff

    onElem' elemIx = withInputInfo $ \ len ->
                     shallowEditorLift $
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

        singleton' = shallowConstructorLift 1 singleton

        onSlice' sliceIx sliceLen = jointInfoEditor crop splice $
                                    withInputInfo $ \ lenDiff ->
                                    shallowEditorLift $
                                    deepEditorLift opsConv $
                                    onSlice (lenDiff - sliceIx) sliceLen

            where

            crop :: Int -> (Int, Int)
            crop len = (sliceLen, len - sliceLen)

            splice :: (Int, Int) -> Int
            splice (sliceLen, lenDiff) = sliceLen + lenDiff

        onElem' elemIx = withInputInfo $ \ len ->
                         shallowEditorLift $
                         onElem (pred len - elemIx)

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

    stdCoreOps = CoreOps empty singleton onSlice onElem

        where

        empty = Seq.empty

        singleton = Seq.singleton <$> wholeConstructor stdOps

        onSlice sliceIx sliceLen = editorMap crop splice $
                                   shallowEditorLift $
                                   wholeEditor stdOps

            where

            crop :: Seq a -> (Seq a, (Seq a, Seq a))
            crop seq = (slice, (prefix, suffix))

                where

                (prefix, rest) = Seq.splitAt sliceIx seq

                (slice, suffix) = Seq.splitAt sliceLen rest

            splice :: (Seq a, (Seq a, Seq a)) -> Seq a
            splice (slice, (prefix, suffix))
                = prefix Seq.>< slice Seq.>< suffix

        onElem elemIx = editorMap crop splice $
                        shallowEditorLift $
                        wholeEditor stdOps

            where

            crop :: Seq a -> (a, (Seq a, Seq a))
            crop seq = (elem, (prefix, suffix))

                where

                (prefix, rest) = Seq.splitAt elemIx seq

                elem Seq.:< suffix = Seq.viewl rest

            splice :: (a, (Seq a, Seq a)) -> Seq a
            splice (elem, (prefix, suffix))
                = prefix Seq.>< elem Seq.<| suffix

-- * Data

instance Data a => Data (Seq a) where

    data CanonicalCoreOps (Seq a) o where

        CanonicalCoreOps :: CoreOperations elemCoreOps
                         => CanonicalCoreOps (Seq (DataOf elemCoreOps))
                                             (CoreOps elemCoreOps)

    coreOpsEqFromCanonicity CanonicalCoreOps CanonicalCoreOps
        = lift coreOpsEq

        where

        lift :: elemCoreOps1 :~~: elemCoreOps2
             -> CoreOps elemCoreOps1 :~~: CoreOps elemCoreOps2
        lift HRefl = HRefl
