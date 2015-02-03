module Data.Incremental.Sequence (

    -- * Changes

    insert,
    delete,
    shift,
    changeAt,

    -- * Atomic changes

    AtomicChange (Insert, Delete, Shift, ChangeAt),
    normalizeAtomicChange,

    -- * Transformations

    map,
    map',
    concat,
    singleton,
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
import           Data.Foldable (asum, toList, foldl)
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

-- * Changes

instance Changeable a => Changeable (Seq a) where

    type StdChange (Seq a) = MultiChange (AtomicChange a)

insert :: Int -> Seq a -> StdChange (Seq a)
insert ix seq = MultiChange.singleton (Insert ix seq)

delete :: Int -> Int -> StdChange (Seq a)
delete ix len = MultiChange.singleton (Delete ix len)

shift :: Int -> Int -> Int -> StdChange (Seq a)
shift src len tgt = MultiChange.singleton (Shift src len tgt)

changeAt :: Int -> StdChange a -> StdChange (Seq a)
changeAt ix change = MultiChange.singleton (ChangeAt ix change)

-- * Atomic changes

data AtomicChange a = Insert !Int (Seq a)
                    | Delete !Int !Int
                    | Shift !Int !Int !Int
                    | ChangeAt !Int (StdChange a)
{-FIXME:
    Are these strictness annotations sensible? Should the sequence be strict?
-}

{-NOTE:
    Change application for sequences is total. It uses forms of saturation to
    achieve this. All the transformations must work correctly also in the
    saturation cases. At the time of writing, they do.

    Now it is not total anymore. ChangeAt application is partial. It fails if
    the normalized index is not in the interval [0 .. len).
-}
instance Changeable a => Change (AtomicChange a) where

    type Value (AtomicChange a) = Seq a

    Insert ix seq' $$ seq = applyInsert ix seq' seq

    Delete ix len $$ seq = applyDelete ix len seq

    Shift src len tgt $$ seq = applyShift src len tgt seq

    ChangeAt ix change $$ seq = ifChangeAtIxOk (Seq.length seq) ix $
                                front >< (change $$ elem) Seq.<| rear where

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

    ix' = normalizeIx totalLen ix

normalizeIx :: Int -> Int -> Int
normalizeIx totalLen ix = (ix `max` 0) `min` totalLen

normalizeIxAndLen :: Int -> Int -> Int -> (Int, Int)
normalizeIxAndLen totalLen ix len = (ix', len') where

        ix' = normalizeIx totalLen ix

        len' = (len `max` 0) `min` (totalLen - ix')

changeLength :: AtomicChange a -> Int -> Int
changeLength (Insert _ seq) totalLength = totalLength + Seq.length seq
changeLength (Delete _ len) totalLength = totalLength + negate len
changeLength (Shift _ _ _)  totalLength = totalLength
changeLength (ChangeAt _ _) totalLength = totalLength
{-NOTE:
    The given change must be normal.

    The function does not check for illegal ChangeAt indexes.
-}

ifChangeAtIxOk :: Int -> Int -> a -> a
ifChangeAtIxOk len ix cont
    | (ix `max` 0) >= len = error "Data.Incremental.Sequence: \
                                  \ChangeAt index out of bounds"
    | otherwise           = cont

-- * Transformations

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
            elemProp <- fmap (flip Seq.index ix) (readSTRef elemPropsRef)
            change' <- elemProp change
            return (ChangeAt ix change')
    return (seq', prop))

map' :: (Changeable a, StdChange a ~ PrimitiveChange a,
        Changeable b, StdChange b ~ PrimitiveChange b) =>
       (a -> b) -> Seq a ->> Seq b
map' fun = MultiChange.map $ statelessTrans (fmap fun) prop where

    prop (Insert ix seq)      = Insert ix (fmap fun seq)
    prop (Delete ix len)      = Delete ix len
    prop (Shift src len tgt)  = Shift src len tgt
    prop (ChangeAt ix change) = ChangeAt ix (fmap fun change)

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

concat :: Changeable a => Seq (Seq a) ->> Seq a
concat = MultiChange.bind $ stateTrans init prop where

    init seq = (concatSeq seq, seqToConcatState seq)

    prop (Insert ix seq) state = (change', state') where

        (ix', front, rear) = splitAndTranslate ix state

        change' = insert ix' (concatSeq seq)

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

    prop (ChangeAt ix change) state = ifChangeAtIxOk len ix $
                                      (change', state') where

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
                        -> ifChangeAtIxOk curElemLen elemIx $
                           ChangeAt (ix' + elemIx) change

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

-- ** Monadic structure

singleton :: Changeable a => a ->> Seq a
singleton = statelessTrans Seq.singleton (changeAt 0)

concatMap :: (Changeable a, Changeable b) => (a ->> Seq b) -> Seq a ->> Seq b
concatMap trans = concat . map trans

-- ** Gates

gate :: Changeable a => (a ->> Bool) -> a ->> Seq a
gate prd = stTrans (\ val -> do
    ref <- newSTRef val
    ~(accepted, prop) <- toSTProc (sanitize . prd) val
    let prop' change = do
            oldVal <- readSTRef ref
            let newVal = change $$ oldVal
            writeSTRef ref newVal
            acceptedChange <- prop change
            return $ case acceptedChange of
                Keep          -> mempty
                Replace False -> delete 0 1
                Replace True  -> insert 0 (Seq.singleton newVal)
    return (emptyOrSingleton accepted val, prop'))

gate' :: (Changeable a, StdChange a ~ PrimitiveChange a) => (a -> Bool) -> a ->> Seq a
gate' prd = stateTrans init prop where

    init val = (emptyOrSingleton accepted val, accepted) where

        accepted = prd val

    prop Keep          accepted = (mempty, accepted)
    prop (Replace val) accepted = case (accepted, prd val) of
        (False, True) -> (insert 0 (Seq.singleton val), True)
        (True, False) -> (delete 0 1, False)
        _             -> (mempty, accepted)

emptyOrSingleton :: Bool -> a -> Seq a
emptyOrSingleton accepted val | accepted  = Seq.singleton val
                              | otherwise = Seq.empty

-- ** Filtering

filter :: Changeable a => (a ->> Bool) -> Seq a ->> Seq a
filter = concatMap . gate

filter' :: (Changeable a, StdChange a ~ PrimitiveChange a) =>
           (a -> Bool) -> Seq a ->> Seq a
filter' = concatMap . gate'

-- ** Reversal

reverse :: Changeable a => Seq a ->> Seq a
reverse = MultiChange.map $ stateTrans init prop where

    init seq = (Seq.reverse seq, Seq.length seq)

    prop change state = propNorm (normalizeAtomicChange state change) state

    propNorm (Insert ix seq) state = (change', state') where

        change' = Insert (state - ix) (Seq.reverse seq)

        state' = state + Seq.length seq

    propNorm (Delete ix len) state = (change', state') where

        change' = Delete (state - (ix + len)) len

        state' = state - len

    propNorm (Shift src len tgt) state = (change', state') where

        change' = Shift (state - (src + len)) len (state - len - tgt)

        state' = state

    propNorm (ChangeAt ix elemChange) state = (change', state') where

        change' = ChangeAt (state - ix - 1) elemChange

        state' = state

-- ** Sorting

sort :: (Ord a, Changeable a) => Seq a ->> Seq a
sort = MultiChange.bind $ orderTSTTrans (\ seq -> do
    let seq' = Seq.sort seq
    taggedSeq <- traverse (\ elem -> fmap ((,) elem) insertMaximum) seq
    let taggedElemSet = Set.fromList (toList taggedSeq)
    taggedSeqRef <- lift $ newSTRef taggedSeq
    taggedElemSetRef <- lift $ newSTRef taggedElemSet
    let performInsert ix elem = do
            taggedSeq <- lift $ readSTRef taggedSeqRef
            let (front, rest) = Seq.splitAt ix taggedSeq
            tag <- case Seq.viewl rest of
                       Seq.EmptyL                   -> insertMaximum
                       (_, neighborTag) Seq.:< rear -> insertBefore neighborTag
            lift $ writeSTRef taggedSeqRef (front >< (elem, tag) Seq.<| rest)
            oldTaggedElemSet <- lift $ readSTRef taggedElemSetRef
            let newTaggedElemSet = Set.insert (elem, tag) taggedElemSet
            lift $ writeSTRef taggedElemSetRef newTaggedElemSet
            return (Set.findIndex (elem, tag) newTaggedElemSet)
    let performDelete ix = do
            taggedSeq <- lift $ readSTRef taggedSeqRef
            let (front, rest) = Seq.splitAt ix taggedSeq
            let (elem, tag) Seq.:< rear = Seq.viewl rest
            lift $ writeSTRef taggedSeqRef (front >< rear)
            taggedElemSet <- lift $ readSTRef taggedElemSetRef
            lift $ writeSTRef taggedElemSetRef (Set.delete (elem, tag) taggedElemSet)
            return (Set.findIndex (elem, tag) taggedElemSet)
    let elemInsert ix elem = do
            ix' <- performInsert ix elem
            return (Insert ix' (Seq.singleton elem))
    let elemDelete ix = do
            ix' <- performDelete ix
            return (Delete ix' 1)
    let elemShift src tgt = do
            taggedSeq <- lift $ readSTRef taggedSeqRef
            let elem = fst (Seq.index taggedSeq src)
            src' <- performDelete src
            tgt' <- performInsert tgt elem
            return (Shift src' 1 tgt')
    let propNorm (Insert ix seq) = do
            changes' <- traverse (elemInsert ix) (Prelude.reverse (toList seq))
            return (MultiChange.fromList changes')
        propNorm (Delete ix len) = do
            changes' <- traverse elemDelete (replicate len ix)
            return (MultiChange.fromList changes')
        propNorm (Shift src len tgt) = do
            changes' <- traverse (elemShift src) [tgt .. tgt + len - 1]
            return (MultiChange.fromList changes')
        propNorm (ChangeAt ix change) = do
            taggedSeq <- lift $ readSTRef taggedSeqRef
            let (oldElem, _) = Seq.index taggedSeq ix
            let newElem = change $$ oldElem
            src' <- performDelete ix
            tgt' <- performInsert ix newElem
            return (shift src' 1 tgt' `mappend` changeAt src' change)
    let prop change = do
            taggedSeq <- lift $ readSTRef taggedSeqRef
            propNorm (normalizeAtomicChange (Seq.length taggedSeq) change)
    return (seq', prop))

orderTSTTrans :: (forall o s . TransProc (OrderT o (ST s)) p q) -> Trans p q
orderTSTTrans transProc = trans (\ cont -> runST (evalOrderT (cont transProc)))

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

toOrderValue :: Changeable a => (a -> a -> Ordering) -> a ->> OrderValue a
toOrderValue compare = statelessTrans (OrderValue compare) OrderChange

fromOrderValue :: Changeable a => OrderValue a ->> a
fromOrderValue = statelessTrans (\ (OrderValue _ val) -> val)
                                (\ (OrderChange change) -> change)

newtype OrderChange p = OrderChange p deriving Monoid

instance Change p => Change (OrderChange p) where

    type Value (OrderChange p) = OrderValue (Value p)

    OrderChange change $$ OrderValue compare val = OrderValue compare $
                                                   change $$ val

instance Changeable a => Changeable (OrderValue a) where

    type StdChange (OrderValue a) = OrderChange (StdChange a)
