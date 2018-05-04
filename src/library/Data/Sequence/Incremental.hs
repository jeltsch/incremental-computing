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
import           Data.Type.Equality ((:~~:) (HRefl))
import           Data.FingerTree (FingerTree, Measured (measure))
import qualified Data.FingerTree as FingerTree
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Incremental

-- * Transformations

-- FIXME: Check strictness of tuples.

{-NOTE:
    Once we have type application patterns, we can get rid of the helper
    variables opsConv0, opsConv1, etc.
-}

-- ** Concatenation

concat :: Seq (Seq a) ->> Seq a
concat = infoTrans @ConcatInfo opsConv0

    where

    opsConv0 :: forall a o0 . (CoreOperations o0, DataOf o0 ~ Seq (Seq a))
             => InfoOpsConv ConcatInfo (Seq a) o0
    opsConv0 = case canonicalCoreOps @_ @o0 of CanonicalCoreOps -> opsConv1

    opsConv1 :: forall a o1 . (CoreOperations o1, DataOf o1 ~ Seq a)
             => InfoOpsConv ConcatInfo (Seq a) (CoreOps o1)
    opsConv1 = case canonicalCoreOps @_ @o1 of CanonicalCoreOps -> opsConv2

    opsConv2 :: forall a o2 . (CoreOperations o2, DataOf o2 ~ a)
             => InfoOpsConv ConcatInfo (Seq a) (CoreOps (CoreOps o2))
    opsConv2 = InfoOpsConv $ \ (AbstractOps ops) -> AbstractOps (convBase ops)

    convBase :: Ops (CoreOps elemCoreOps)
                    seqInternal
                    seqPacket
                    seq
             -> Ops (CoreOps (CoreOps elemCoreOps))
                    ('Internal seqInternal (seqPacket, Int))
                    (seqPacket, ConcatInfo)
                    (seq, ConcatInfo)
    convBase ops@(Ops { coreOps = CoreOps { .. }, .. })
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
                                    deepEditorLift convBase $
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

instance Semigroup ConcatInfoMeasure where

    ConcatInfoMeasure srcLen1 tgtLen1 <> ConcatInfoMeasure srcLen2 tgtLen2
        = ConcatInfoMeasure (srcLen1 + srcLen2) (tgtLen1 + tgtLen2)

instance Monoid ConcatInfoMeasure where

    mempty = ConcatInfoMeasure 0 0

instance Measured ConcatInfoMeasure ConcatInfoElem where

    measure (ConcatInfoElem elemLen) = ConcatInfoMeasure 1 elemLen

splitConcatInfoAt :: Int -> ConcatInfo -> (ConcatInfo, ConcatInfo)
splitConcatInfoAt ix = FingerTree.split ((> ix) . sourceLength)

-- ** Reversal

-- FIXME: Use lengthOps.

reverse :: Seq a ->> Seq a
reverse = infoTrans @Int opsConv0

    where

    opsConv0 :: forall a o0 . (CoreOperations o0, DataOf o0 ~ Seq a)
             => InfoOpsConv Int (Seq a) o0
    opsConv0 = case canonicalCoreOps @_ @o0 of CanonicalCoreOps -> opsConv1

    opsConv1 :: forall a o1 . (CoreOperations o1, DataOf o1 ~ a)
             => InfoOpsConv Int (Seq a) (CoreOps o1)
    opsConv1 = InfoOpsConv $ \ (AbstractOps ops) -> AbstractOps (convBase ops)

    convBase :: Ops (CoreOps elemCoreOps)
                    seqInternal
                    seqPacket
                    seq
             -> Ops (CoreOps elemCoreOps)
                    seqInternal
                    (seqPacket, Int)
                    (seq, Int)
    convBase (Ops { coreOps = CoreOps { .. }, .. })
        = dynamicInfoOps pack unpack $
          CoreOps empty' singleton' onSlice' onElem'

        where

        empty' = (empty, 0)

        singleton' = shallowConstructorLift 1 singleton

        onSlice' sliceIx sliceLen = jointInfoEditor crop splice $
                                    withInputInfo $ \ lenDiff ->
                                    shallowEditorLift $
                                    deepEditorLift convBase $
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

        singleton = constructorMap Seq.singleton (wholeConstructor stdOps)

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
