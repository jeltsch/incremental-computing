module Data.Incremental.Sequence (

    -- * Type

    Seq,
    {-NOTE:
        By re-exporting Seq, we get the definition of DefaultChange for Seq into
        the documentation generated by Haddock.
    -}

    -- * Changes

    insert,
    delete,
    shift,
    changeAt,

    -- * Atomic changes

    AtomicChange (Insert, Delete, Shift, ChangeAt),
    normalizeAtomicChange,

    -- * Transformations

    singleton,
    fromPair,
    cat,
    null,
    length,
    map,
    map',
    concat,
    concatMap,
    gate,
    gate',
    filter,
    filter',
    reverse,
    sort,
    sortBy

) where

-- Prelude

import Prelude hiding (
    id,
    (.),
    null,
    length,
    map,
    concat,
    concatMap,
    filter,
    reverse,
    foldl)
import qualified Prelude

-- Control

import Control.Category
import Control.Monad.ST.Lazy
import Control.Monad.Trans.Class
import Control.Monad.Trans.Order

-- Data

import           Data.Monoid
import           Data.Foldable (foldl, asum, toList)
import           Data.Traversable (traverse)
import           Data.FingerTree (FingerTree, Measured (measure))
import qualified Data.FingerTree as FingerTree
import           Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.STRef.Lazy
import           Data.MultiChange (MultiChange)
import qualified Data.MultiChange as MultiChange
import           Data.Incremental
import qualified Data.Incremental.Tuple as Tuple

{-NOTE:
    Naming policy:

      • Data of argument transformations gets additional text, like “elem”.

      • Data related to input of a transformation gets an ordinary identifier,
        and the corresponding data related to output gets the same identifier
        with a prime.

      • Data that refers to the situation after applying a change gets an
        identifier that starts with “new”, and the corresponding data that
        refers to the situation before applying this change gets the
        corresponding identifier that starts with “old”.
-}

-- * Changes

instance Changeable a => Changeable (Seq a) where

    type DefaultChange (Seq a) = MultiChange (AtomicChange a)

insert :: Int -> Seq a -> DefaultChange (Seq a)
insert ix seq = MultiChange.singleton (Insert ix seq)

delete :: Int -> Int -> DefaultChange (Seq a)
delete ix len = MultiChange.singleton (Delete ix len)

shift :: Int -> Int -> Int -> DefaultChange (Seq a)
shift src len tgt = MultiChange.singleton (Shift src len tgt)

changeAt :: Int -> DefaultChange a -> DefaultChange (Seq a)
changeAt ix change = MultiChange.singleton (ChangeAt ix change)

-- * Atomic changes

data AtomicChange a = Insert !Int (Seq a)
                    | Delete !Int !Int
                    | Shift !Int !Int !Int
                    | ChangeAt !Int (DefaultChange a)
{-FIXME:
    Are these strictness annotations sensible? Should the sequence be strict?
-}

{-NOTE:
    Change application for sequences is total. It uses forms of saturation to
    achieve this. All the transformations must work correctly also in the
    saturation cases. At the time of writing, they do.
-}
instance Changeable a => Change (AtomicChange a) where

    type Value (AtomicChange a) = Seq a

    Insert ix seq' $$ seq = applyInsert ix seq' seq

    Delete ix len $$ seq = applyDelete ix len seq

    Shift src len tgt $$ seq = applyShift src len tgt seq

    ChangeAt ix change $$ seq
        | indexInBounds (Seq.length seq) ix
            = front >< (change $$ elem) Seq.<| rear
        | otherwise
            = seq where

        (front, rest) = Seq.splitAt ix seq

        (elem Seq.:< rear) = Seq.viewl rest

applyInsert :: Int -> Seq a -> Seq a -> Seq a
applyInsert ix seq' seq = front >< seq' >< rear where

    (front, rear) = Seq.splitAt ix seq

applyDelete :: Int -> Int -> Seq a -> Seq a
applyDelete ix len seq = front >< rear where

    (front, rest) = Seq.splitAt ix seq

    (_, rear) = Seq.splitAt len rest

applyShift :: Int -> Int -> Int -> Seq a -> Seq a
applyShift src len tgt seq = applyInsert tgt mid (front >< rear) where

    (front, rest) = Seq.splitAt src seq

    (mid, rear) = Seq.splitAt len rest

normalizeAtomicChange :: Int -> AtomicChange a -> AtomicChange a
normalizeAtomicChange totalLen (Insert ix seq) = Insert ix' seq where

    ix' = normalizeIx totalLen ix

normalizeAtomicChange totalLen (Delete ix len) = Delete ix' len' where

    (ix', len') = normalizeIxAndLen totalLen ix len

normalizeAtomicChange totalLen (Shift src len tgt) = Shift src' len' tgt' where

    (src', len') = normalizeIxAndLen totalLen src len

    tgt' = normalizeIx (totalLen - len') tgt

normalizeAtomicChange totalLen (ChangeAt ix change) = ChangeAt ix' change where

    ix' | indexInBounds totalLen ix = ix
        | otherwise                 = totalLen

normalizeIx :: Int -> Int -> Int
normalizeIx totalLen ix = (ix `max` 0) `min` totalLen

normalizeIxAndLen :: Int -> Int -> Int -> (Int, Int)
normalizeIxAndLen totalLen ix len = (ix', len') where

        ix' = normalizeIx totalLen ix

        len' = (len `max` 0) `min` (totalLen - ix')

noChange :: Changeable a => AtomicChange a
noChange = ChangeAt (-1) mempty

changeLength :: AtomicChange a -> Int -> Int
changeLength (Insert _ seq) totalLength = totalLength + Seq.length seq
changeLength (Delete _ len) totalLength = totalLength - len
changeLength (Shift _ _ _)  totalLength = totalLength
changeLength (ChangeAt _ _) totalLength = totalLength
-- NOTE: The given change must be normal.

indexInBounds :: Int -> Int -> Bool
indexInBounds len ix = ix >= 0 && ix < len

-- * Transformations

-- ** Singleton construction

singleton :: Changeable a => a ->> Seq a
singleton = simpleTrans Seq.singleton (changeAt 0)

-- ** Two-element sequence construction

fromPair :: Changeable a => (a, a) ->> Seq a
fromPair = MultiChange.map $ simpleTrans fun prop where

    fun ~(val1, val2) = Seq.fromList [val1, val2]

    prop (Tuple.First change)  = ChangeAt 0 change
    prop (Tuple.Second change) = ChangeAt 1 change

-- ** Concatenation of two sequences

cat :: Changeable a => (Seq a, Seq a) ->> Seq a
cat = concat . fromPair

-- ** Length queries

null :: Changeable a => Seq a ->> Bool
null = fromFunction (== 0) . length

length :: Changeable a => Seq a ->> Int
length = MultiChange.composeMap $ stateTrans init prop where

    init seq = (len, len) where

        len = Seq.length seq

    prop change state = (ReplaceBy len', len') where

        normChange = normalizeAtomicChange state change

        len' = changeLength normChange state

-- ** Mapping

map :: (Changeable a, Changeable b) => (a ->> b) -> Seq a ->> Seq b
map trans = MultiChange.map $ stTrans (\ seq -> do
    let elemProc = toSTProc trans
    let seqInit seq = do
            procOutputs <- traverse elemProc seq
            return (fmap fst procOutputs, fmap snd procOutputs)
    (seq', elemProps) <- seqInit seq
    elemPropsRef <- newSTRef elemProps
    let prop (Insert ix seq) = do
            (seq', elemProps) <- seqInit seq
            modifySTRef elemPropsRef (applyInsert ix elemProps)
            return (Insert ix seq')
        prop (Delete ix len) = do
            modifySTRef elemPropsRef (applyDelete ix len)
            return (Delete ix len)
        prop (Shift src len tgt) = do
            modifySTRef elemPropsRef (applyShift src len tgt)
            return (Shift src len tgt)
        prop (ChangeAt ix change) = do
            elemProps <- readSTRef elemPropsRef
            if indexInBounds (Seq.length elemProps) ix
                then do
                    let elemProp = Seq.index elemProps ix
                    change' <- elemProp change
                    return (ChangeAt ix change')
                else return noChange
    return (seq', prop))

map' :: (Changeable a, DefaultChange a ~ PrimitiveChange a,
         Changeable b, DefaultChange b ~ PrimitiveChange b) =>
       (a -> b) -> Seq a ->> Seq b
map' fun = MultiChange.map $ simpleTrans (fmap fun) prop where

    prop (Insert ix seq)      = Insert ix (fmap fun seq)
    prop (Delete ix len)      = Delete ix len
    prop (Shift src len tgt)  = Shift src len tgt
    prop (ChangeAt ix change) = ChangeAt ix (fmap fun change)

-- ** Concatenation of multiple sequences

seqConcat :: Seq (Seq a) -> Seq a
seqConcat = asum

newtype ConcatStateElement = ConcatStateElement Int

data ConcatStateMeasure = ConcatStateMeasure {
                              sourceLength :: !Int,
                              targetLength :: !Int
                          }

instance Monoid ConcatStateMeasure where

    mempty = ConcatStateMeasure 0 0

    mappend (ConcatStateMeasure srcLen1 tgtLen1)
            (ConcatStateMeasure srcLen2 tgtLen2) = measure' where

        measure' = ConcatStateMeasure (srcLen1 + srcLen2) (tgtLen1 + tgtLen2)

instance Measured ConcatStateMeasure ConcatStateElement where

    measure (ConcatStateElement elemLen) = ConcatStateMeasure 1 elemLen

type ConcatState = FingerTree ConcatStateMeasure ConcatStateElement

seqToConcatState :: Seq (Seq a) -> ConcatState
seqToConcatState = FingerTree.fromList .
                   toList              .
                   fmap (ConcatStateElement . Seq.length)

concat :: Changeable a => Seq (Seq a) ->> Seq a
concat = MultiChange.bind $ stateTrans init prop where

    init seq = (seqConcat seq, seqToConcatState seq)

    prop (Insert ix seq) state = (change', state') where

        (ix', front, rear) = splitAndTranslate ix state

        change' = insert ix' (seqConcat seq)

        state' = front <> seqToConcatState seq <> rear

    prop (Delete ix len) state = (change', state') where

        (ix', front, rest) = splitAndTranslate ix state

        (len', _, rear) = splitAndTranslate len rest

        change' = delete ix' len'

        state' = front <> rear

    prop (Shift src len tgt) state = (change', state') where

        (src', front, rest) = splitAndTranslate src state

        (len', mid, rear) = splitAndTranslate len rest

        (tgt', front', rear') = splitAndTranslate tgt (front <> rear)

        change' = shift src' len' tgt'

        state' = front' <> mid <> rear'

    prop (ChangeAt ix change) state
        | indexInBounds len ix = (change', state')
        | otherwise            = (mempty, state) where

        len = sourceLength (measure state)

        (ix', front, rest) = splitAndTranslate ix state

        (ConcatStateElement elemLen FingerTree.:< rear) = FingerTree.viewl rest

        (change', elemLen') = foldl next (mempty, elemLen) change where

            next (curChange, curElemLen) atomic = (curChange', curElemLen') where

                normAtomic = normalizeAtomicChange curElemLen atomic

                shiftedNormAtomic = case normAtomic of
                    Insert elemIx seq
                        -> Insert (ix' + elemIx) seq
                    Delete elemIx curElemLen
                        -> Delete (ix' + elemIx) curElemLen
                    Shift elemSrc curElemLen elemTgt
                        -> Shift (ix' + elemSrc) curElemLen (ix' + elemTgt)
                    ChangeAt elemIx change
                        -> if indexInBounds curElemLen elemIx
                               then ChangeAt (ix' + elemIx) change
                               else noChange

                curChange' = MultiChange.singleton shiftedNormAtomic `mappend`
                             curChange

                curElemLen' = changeLength normAtomic curElemLen
        -- NOTE: Strictness is not perfect.
        -- FIXME: One line too wide.

        state' = front <> (ConcatStateElement elemLen' FingerTree.<| rear)
        {-NOTE:
            This is a bit fishy. Even if the inner change is illegal, we get a
            non-⊥ state. So the state is not always a property of the original
            value. If the original value is ⊥, the state might not be ⊥.
            However, this should not result in violation of the main property
            that changing and then transforming is the same as transforming and
            then changing with the propagated change. We would propagate to
            non-⊥ changes in the future, but applying these to ⊥ yields ⊥. The
            latter might not always be the case, but it is the case for
            sequences.
        -}

    splitAndTranslate :: Int -> ConcatState -> (Int, ConcatState, ConcatState)
    splitAndTranslate ix state = (ix', front, rear) where

        (front, rear) = FingerTree.split ((> ix) . sourceLength) state

        ix' = targetLength (measure front)

-- ** Monadic bind

concatMap :: (Changeable a, Changeable b) => (a ->> Seq b) -> Seq a ->> Seq b
concatMap trans = concat . map trans

-- ** Gates

gate :: Changeable a => (a ->> Bool) -> a ->> Seq a
gate prd = stTrans (\ val -> do
    valRef <- newSTRef val
    ~(accepted, prop) <- toSTProc prd val
    acceptedRef <- newSTRef accepted
    let prop' change = do
            oldVal <- readSTRef valRef
            let newVal = change $$ oldVal
            writeSTRef valRef newVal
            acceptedChange <- prop change
            oldAccepted <- readSTRef acceptedRef
            let newAccepted = acceptedChange $$ oldAccepted
            writeSTRef acceptedRef newAccepted
            return $ case (oldAccepted, newAccepted) of
                (False, False) -> mempty
                (False, True)  -> insert 0 (Seq.singleton newVal)
                (True,  False) -> delete 0 1
                (True,  True)  -> changeAt 0 change
    return (emptyOrSingleton accepted val, prop'))
{-FIXME:
    Consider factoring out at least the update of values and accepted flags.
-}
{-FIXME:
    Here we seem to use the apostrophe to distinguish between argument
    transformation and result transformation, which does not seem to be coherent
    with the rest of this module.
-}
{-FIXME:
    The gate transformation is properly strict regarding the accepted flag. It
    is currently not clear to us, whether we should also make the stored current
    source value reduce when the target value or a target change is reduced.
    This might make sense to avoid changes to the source value to accumulate.
    However, we do not know anything about the source value type, in particular,
    nothing about how much of evaluation is triggered by WHNF reduction.
-}

gate' :: (Changeable a, DefaultChange a ~ PrimitiveChange a) =>
         (a -> Bool) -> a ->> Seq a
gate' prd = stateTrans init prop where

    init val = (emptyOrSingleton accepted val, accepted) where

        accepted = prd val

    prop Keep            oldAccepted = (mempty,  oldAccepted)
    prop (ReplaceBy val) oldAccepted = (change', newAccepted) where

        change' = case (oldAccepted, newAccepted) of
                      (False, False) -> mempty
                      (False, True)  -> insert 0 (Seq.singleton val)
                      (True,  False) -> delete 0 1
                      (True,  True)  -> changeAt 0 (ReplaceBy val)

        newAccepted = prd val

emptyOrSingleton :: Bool -> a -> Seq a
emptyOrSingleton accepted val | accepted  = Seq.singleton val
                              | otherwise = Seq.empty

-- ** Filtering

filter :: Changeable a => (a ->> Bool) -> Seq a ->> Seq a
filter = concatMap . gate

filter' :: (Changeable a, DefaultChange a ~ PrimitiveChange a) =>
           (a -> Bool) -> Seq a ->> Seq a
filter' = concatMap . gate'

-- FIXME: Maybe add partition and partition'.

-- ** Reversal

reverse :: Changeable a => Seq a ->> Seq a
reverse = MultiChange.map $ stateTrans init prop where

    init seq = (Seq.reverse seq, Seq.length seq)

    prop change state = propNorm (normalizeAtomicChange state change) state

    propNorm change state = (propCore change state, changeLength change state)

    propCore (Insert ix seq) state = change' where

        change' = Insert (state - ix) (Seq.reverse seq)

    propCore (Delete ix len) state = change' where

        change' = Delete (state - (ix + len)) len

    propCore (Shift src len tgt) state = change' where

        change' = Shift (state - (src + len)) len (state - len - tgt)

    propCore (ChangeAt ix elemChange) state = change' where

        change' = ChangeAt (state - ix - 1) elemChange

-- ** Sorting

{-FIXME:
    The strictness policy for sort is that evaluation of the state, except for
    the elements in it, is triggered when the initial target value or a target
    change is reduced to WHNF. We currently achieve the desired strictness by
    the following means:

      • Tagged is strict in the tag.

      • The initial target value depends on initTaggedSet via seq.

      • Target changes depend on the current tagged sequence and tagged set via
        seq.
-}

data Tagged o val = Tagged val !(Element o) deriving (Eq, Ord)

sort :: (Ord a, Changeable a) => Seq a ->> Seq a
sort = MultiChange.bind $ orderSTTrans (\ seq -> do
    let seq' = Seq.sort seq
    initTaggedSeq <- traverse (\ elem -> fmap (Tagged elem) newMaximum) seq
    let initTaggedSet = Set.fromList (toList initTaggedSeq)
    taggedSeqRef <- lift $ newSTRef initTaggedSeq
    taggedSetRef <- lift $ newSTRef initTaggedSet
    let performInsert ix elem = do
            taggedSeq <- lift $ readSTRef taggedSeqRef
            let (front, rest) = Seq.splitAt ix taggedSeq
            tag <- case Seq.viewl rest of
                       Seq.EmptyL                    -> newMaximum
                       Tagged _ neighborTag Seq.:< _ -> newBefore neighborTag
            lift $ writeSTRef taggedSeqRef
                              (front >< Tagged elem tag Seq.<| rest)
            oldTaggedSet <- lift $ readSTRef taggedSetRef
            let newTaggedSet = Set.insert (Tagged elem tag) oldTaggedSet
            lift $ writeSTRef taggedSetRef newTaggedSet
            return (Set.findIndex (Tagged elem tag) newTaggedSet)
    let performDelete ix = do
            taggedSeq <- lift $ readSTRef taggedSeqRef
            let (front, rest) = Seq.splitAt ix taggedSeq
            let Tagged elem tag Seq.:< rear = Seq.viewl rest
            lift $ writeSTRef taggedSeqRef (front >< rear)
            taggedSet <- lift $ readSTRef taggedSetRef
            lift $ writeSTRef taggedSetRef
                              (Set.delete (Tagged elem tag) taggedSet)
            return (Set.findIndex (Tagged elem tag) taggedSet)
    let elemInsert ix elem = do
            ix' <- performInsert ix elem
            return (Insert ix' (Seq.singleton elem))
    let elemDelete ix = do
            ix' <- performDelete ix
            return (Delete ix' 1)
    let elemShift src tgt = do
            taggedSeq <- lift $ readSTRef taggedSeqRef
            let Tagged elem _ = Seq.index taggedSeq src
            src' <- performDelete src
            tgt' <- performInsert tgt elem
            return (Shift src' 1 tgt')
    let propCore (Insert ix seq) = do
            atomics' <- traverse (elemInsert ix) (Prelude.reverse (toList seq))
            return (MultiChange.fromList atomics')
        propCore (Delete ix len) = do
            atomics' <- traverse elemDelete (replicate len ix)
            return (MultiChange.fromList atomics')
        propCore (Shift src len tgt) = (case compare src tgt of
            LT -> genShifts (Prelude.reverse [0 .. len - 1])
            GT -> genShifts [0 .. len - 1]
            EQ -> return mempty) where

            genShifts offsets = do
                atomics' <- traverse genShift offsets
                return (MultiChange.fromList atomics')

            genShift offset = elemShift (src + offset) (tgt + offset)

        propCore (ChangeAt ix change) = do
            taggedSeq <- lift $ readSTRef taggedSeqRef
            if indexInBounds (Seq.length taggedSeq) ix
                then do
                    let Tagged oldElem _ = Seq.index taggedSeq ix
                    let newElem = change $$ oldElem
                    src' <- performDelete ix
                    tgt' <- performInsert ix newElem
                    return (shift src' 1 tgt' `mappend` changeAt src' change)
                else return mempty
    let prop change = do
            taggedSeq <- lift $ readSTRef taggedSeqRef
            change' <- propCore $
                       normalizeAtomicChange (Seq.length taggedSeq) change
            taggedSeq <- lift $ readSTRef taggedSeqRef
            taggedSet <- lift $ readSTRef taggedSetRef
            return (taggedSeq `Prelude.seq` taggedSet `Prelude.seq` change')
    return (initTaggedSet `Prelude.seq` seq', prop))

orderSTTrans :: (forall o s . TransProc (OrderT o (ST s)) p q) -> Trans p q
orderSTTrans transProc = trans (\ cont -> runST (evalOrderT (cont transProc)))

sortBy :: Changeable a => (a -> a -> Ordering) -> Seq a ->> Seq a
sortBy compare = map fromOrderValue . sort . map (toOrderValue compare)
{-FIXME:
    In the future, we maybe should have a sortBy that takes a compare
    transformation instead of a compare function.
-}

data OrderValue a = OrderValue (a -> a -> Ordering) a

instance Eq (OrderValue a) where

    orderVal1 == orderVal2 = compare orderVal1 orderVal2 == EQ

instance Ord (OrderValue a) where

    compare (OrderValue compare val1) (OrderValue _ val2) = compare val1 val2

newtype OrderChange p = OrderChange p deriving Monoid

instance Change p => Change (OrderChange p) where

    type Value (OrderChange p) = OrderValue (Value p)

    OrderChange change $$ OrderValue compare val = OrderValue compare $
                                                   change $$ val

instance Changeable a => Changeable (OrderValue a) where

    type DefaultChange (OrderValue a) = OrderChange (DefaultChange a)

toOrderValue :: Changeable a => (a -> a -> Ordering) -> a ->> OrderValue a
toOrderValue compare = simpleTrans (OrderValue compare) OrderChange

fromOrderValue :: Changeable a => OrderValue a ->> a
fromOrderValue = simpleTrans (\ (OrderValue _ val) -> val)
                             (\ (OrderChange change) -> change)
