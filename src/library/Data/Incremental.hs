module Data.Incremental (

    -- * Changes

    Change (Value, ($$)),
    PrimitiveChange (Keep, Replace),

    -- * Transformations

    Trans,
    TransProc,

    -- ** Construction

    simpleTrans,
    stateTrans,
    stTrans,
    trans,

    -- ** Deconstruction

    runTrans,
    toFunction,
    toSTProc,

    -- ** Utilities

    fromFunction,
    sanitize,

    -- * Changeables

    Changeable (StdChange),
    type (->>)

) where

-- Prelude

import Prelude hiding (id, (.))

-- Control

import Control.Category
import Control.Monad.ST.Lazy
import Control.Monad.ST.Lazy.Unsafe

-- Data

import Data.Monoid
import Data.Functor.Identity
import Data.STRef.Lazy

infixr 0 $$
infixr 0 ->>

-- * Changes

class Change p where

    type Value p :: *

    -- NOTE: Operator $$ is at least not used in the base library.
    ($$) :: p -> Value p -> Value p

data PrimitiveChange a = Keep | Replace a

instance Functor PrimitiveChange where

    fmap _   Keep          = Keep
    fmap fun (Replace val) = Replace (fun val)

instance Monoid (PrimitiveChange a) where

    mempty = Keep

    Keep        `mappend` change = change
    Replace val `mappend` _      = Replace val

instance Change (PrimitiveChange a) where

    type Value (PrimitiveChange a) = a

    Keep        $$ val = val
    Replace val $$ _   = val

-- * Transformations

newtype Trans p q = Trans ((Value p, [p]) -> (Value q, [q]))

instance Category Trans where

    id = Trans id

    Trans conv2 . Trans conv1 = Trans (conv2 . conv1)

{- FIXME:
    Consider implementing a (&&&) and a const (or drop, that is, const ())
    for Trans.
-}

type TransProc m p q = Value p -> m (Value q, p -> m q)

-- ** Construction

simpleTrans :: (Value p -> Value q) -> (p -> q) -> Trans p q
simpleTrans valFun changeFun = trans (\ cont -> runIdentity (cont init)) where

    init val = return (valFun val, return . changeFun)

stateTrans :: (Value p -> (Value q, s)) -> (p -> s -> (q, s)) -> Trans p q
stateTrans pureInit pureProp = stTrans (\ val -> do
    let (val', initState) = pureInit val
    stateRef <- newSTRef initState
    let prop change = do
            oldState <- readSTRef stateRef
            let (change', newState) = pureProp change oldState
            writeSTRef stateRef newState
            return change'
    return (val', prop))

stTrans :: (forall s . TransProc (ST s) p q) -> Trans p q
stTrans init = trans (\ cont -> runST (cont init))

{-NOTE:
    ST with OrderT layers around can be run as follows:

        transNested :: (forall o1 ... on s .
                        TransProc (OrderT o1 (... (OrderT on (ST s)))) p q)
                    -> Trans p q
        transNested proc = trans (\ cont -> runST (
                                            evalOrderT (
                                            ... (
                                            evalOrderT (cont proc)))))
-}

{-FIXME:
    We have to mention in the documentation that the monad is supposed to be
    lazy. If it is strict, the constructed transformation trans will (probably)
    have the following properties:

      • Reducing any expression runTrans trans valAndChanges to WHNF results in
        the initialization being run and the constructed propagator being run on
        all the changes.

      • The expression toSTProc trans is a processor that always yields ⊥ as the
        output value and constructs propagators that always yield ⊥ as the
        output change.
-}
trans :: (forall r . (forall m . Monad m => TransProc m p q -> m r) -> r)
      -> Trans p q
trans cpsInitAndRun = Trans conv where

    conv valAndChanges = cpsInitAndRun $
                         \ init -> monadicConv init valAndChanges

    monadicConv init ~(val, changes) = do
        ~(val', prop) <- init val
        changes' <- mapM prop changes
        return (val', changes')

-- ** Deconstruction

runTrans :: Trans p q -> (Value p, [p]) -> (Value q, [q])
runTrans (Trans conv) = conv

toFunction :: Trans p q -> (Value p -> Value q)
toFunction (Trans conv) val = fst (conv (val, undefined))

{-FIXME:
    We have to mention the following in the documentation:

        The function toSTProc . stTrans is not the identity. A computation in
        the original value of type forall s . TransProc (ST s) may yield an
        undefined state, but for computations in the constructed value,
        undefinedness can only occur in the values they output.

        On the other hand, stTrans . toSTProc is the identity. [At least, it
        should be.]
-}
toSTProc :: Trans p q -> TransProc (ST s) p q
toSTProc (Trans conv) val = do
    (chan, changes) <- newChannel
    let (val', changes') = conv (val, changes)
    remainderRef <- newSTRef changes'
    let prop change = do
            writeChannel chan change
            (next : further) <- readSTRef remainderRef
            writeSTRef remainderRef further
            return next
    return (val', prop)

-- ** Utilities

fromFunction :: (a -> b) -> Trans (PrimitiveChange a) (PrimitiveChange b)
fromFunction fun = simpleTrans fun (fmap fun)

sanitize :: Eq a => Trans (PrimitiveChange a) (PrimitiveChange a)
sanitize = stateTrans init prop where

    init val = (val, val)

    prop Keep          state = (Keep, state)
    prop (Replace val) state = if val == state
                                   then (Keep, state)
                                   else (Replace val, val)

-- * Changeables

class (Monoid (StdChange a), Change (StdChange a), Value (StdChange a) ~ a) =>
      Changeable a where

    type StdChange a :: *
    type StdChange a = PrimitiveChange a

instance Changeable Bool

{-FIXME:
    Add default instance declarations for all remaining Prelude types and
    replace them by something more decent if there is something more decent.
-}

type a ->> b = Trans (StdChange a) (StdChange b)

-- * Channels in the ST monad

data Cell s a = Cell a (CellRef s a)

type CellRef s a = STRef s (Cell s a)

type Channel s a = STRef s (CellRef s a)

newChannel :: ST s (Channel s a, [a])
newChannel = do
    cellRef <- newSTRef undefined
    chan <- newSTRef cellRef
    let getContents cellRef = unsafeInterleaveST $ do
            Cell val cellRef' <- readSTRef cellRef
            vals <- getContents cellRef'
            return (val : vals)
            -- Is this use of unsafeInterleaveST safe?
    contents <- getContents cellRef
    return (chan, contents)

writeChannel :: Channel s a -> a -> ST s ()
writeChannel chan val = do
    cellRef <- readSTRef chan
    cellRef' <- newSTRef undefined
    writeSTRef cellRef (Cell val cellRef')

{-FIXME:
    Is there already an implementation of ST channels?
-}

{-FIXME:
    Remove Control.Monad.ST.Lazy.Unsafe from the import list, if the channel
    code moves to its own module.
-}

{-FIXME:
    The following things are to be considered:

      • At some point, we should also have a PartitionT with an interface
        analogous to OrderT that works with partitions using union–find. We
        should also have a class that covers OrderT, PartitionT and similar data
        constructors and offers a run method. Based on this, we should implement
        trans operations for different nesting depths of things like OrderT and
        PartitionT with ST as the base and maybe also Identity as the base.

        Well, maybe this does not work, because partitions do not allow for
        branching, but branching is inherent in lifting.

      • A possible title for our paper is “On Functional Incremental Computing
        with an Application to Stable Sorting”.

      • Did Acar really implement order maintenance using the approach by Dietz
        and Sleator (1987)? Did Magnus Carlsson did so?

      • Does our framework correspond to update lenses? How is it related to
        update lenses? Look at the slides of Tarmo’s seminar talk from
        11 September 2014.

      • Maybe change the package name from incremental-computation to
        incremental, because there are also packages computations and
        resourceful-computations, both package’s names having an s at the end,
        which could confuse users.

      • Implement maps using finger trees:

          – Subsection 4.7 of the finger trees paper already explains the
            essential things. We should reference this subsection in our paper.

          – We should implement all the operations of Data.Map, including the
            generic mergeWithKey function, but we should implement union in
            terms of mergeWithKey only if this results in the efficient merging
            algorithm shown in the finger trees paper.

          – We should take splitting at a key and union as the primitive map
            changes.

      • Make Data.Map.FingerTree.Map an instance of Changeable, where the basic
        changes are splitLookup and union.

      • Our work on order maintenance could be turned into a paper. Currently,
        one has to read more than one paper to understand the algorithm (Dietz
        and Sleator 1987; Willard 1986) and Dietz and Sleator (1987) do not
        explain deletion.

      • Search trees for implementing incremental sorting:

          – We implement search trees in a purely functional style, except that
            we maintain reference cells with up pointers that point to other
            such reference cells.

          – Modifications of a search tree is done in the purely functional way,
            except that we additionally return a list of necessary up pointer
            changes, which can then be imperatively processed in a separate
            step.

          – The search tree for the original sequence needs support for up
            pointers and uses purely functional comparisons. The search trees
            for the sequences of equivalent elements do not need support for up
            pointers and use comparisons in the ST monad. We should implement a
            single, parameterized version of search trees that captures both
            cases.

          – It might be a good idea to split changes of the tree into two parts:
            a first part that needs comparisons and a second part that need to
            provide information about up pointer changes (is such a split
            possible in the case of deletion?). The first part would need
            monadic operations, but the second would only need applicative
            functor operations. As the intermediate data structure for linking
            the first and the second part, we could use a zipper.

      • Approach for sequence sorting with support for incremental updates:

          – If we want the ordering given by an Ord instance:

             1. Transform the sequence of type Seq a to a value of type
                Map a (Seq a) that has the elements of the sequence as keys (up
                to equality) and maps each key to the elements from the original
                sequence that are equal to the key (in the order as they appear
                in the original sequence).

             2. Transform the map to a sequence in the obvious way.

          – If we want to use a different comparison function c:

              * We define type WithCompare as follows:

                    data WithCompare a = WithCompare a (a -> Ordering)

                    instance Ord (WithCompare a) where

                        compare (WithCompare _ comp1) (WithCompare val2 _)
                            = comp1 val2

              * We map the elements of the original sequence with the function
                \ el -> WithCompare el (compare el). Then we use the above
                facilities for sorting according to an Ord instance and finally
                map again to extract the actual elements from the WithCompare
                values.

            Note that this approach is reminiscent of Conal Elliott’s
            implementation of improving times in “Push-Pull Functional Reactive
            Programming”.

      • The conversion from sequences to maps might be implementable as follows:

          – Use the multiset of the elements of the sequence as the state.

          – For each change, track for which sequence elements (map keys), the
            corresponding sequence of elements (map value) has to change. We
            identify nested tuples with their left-to-right concatenations, so
            that, for example, splitting and concatenation are not considered to
            cause changes. We need Cartesian to be represented as an SMCC
            structure plus duplication and destruction.

            Duplicate:
                All elements of the input sequence have changes.

            Destroy:
                All elements of the input sequence have changes.

            Bimap:
                All elements that have changes on the left or on the right side
                change have changes.

            AssocLeft, AssocRight:
                Nothing changes.

            SplitAt, Cat:
                Nothing changes.

            Const:
                All elements in the constant sequence have changes.

          – Generate value changes for those keys that need changes. Note that
            each key k is mapped to filter (== k) of the original sequence.
            However, our current implementation of filter would mean quadratic
            cost initially and linear cost on every update that introduces a new
            key, as the state for any filter (== k) sequence is of the same size
            as the original sequence. A solution is to use a more compact
            representation of the state, where intervals of zeros (corresponding
            to filtered-out elements) are represented as a single finger tree
            element.

            Maybe we should use such a representation for the implementation of
            concat. Maybe we should actually pack every sequence of equal
            element sequence lengths into a single finger tree element. There
            might be several cases of concat applications where this is useful:

              * In the case of filter, there can be several elements in a row
                that either do or do not satisfy the filter predicate.

              * It can happen that we want to concat a sequence of sequences
                that have equal size.

            We cannot use filter in the implementation of the sequence-to-map
            conversion, since this would require us to initially filter the
            original sequence with (== k) for every key k. However, we can reuse
            the change propagation part of filter.

      • The incrementalized version of maps cannot allow conversion to sequences
        of key–value pairs, but only to sequences of values, because if the map
        was created from a sequence and was then converted to a sequence of
        key–value pairs, the choice of keys from equivalence classes of keys
        would depend on the history of changes to the original sequence, not
        just on the current value of the sequence.

      • Ideas for generic derivation of Changeable implementations:

          – We somehow bake Changeable implementations for arbitrary sums and
            arbitrary products.

          – We should possibly also have a Changeable implementation for pairs
            whose elements have the same type. This implementation should allow
            for swapping the elements.

          – We do not specifically support fixed point types. Instead we just
            use the implementations for sums, products, sequences, and sets and
            apply them recursively. The subtrees of a nonempty binary tree, for
            example, would just be elements of a pair and would thus be
            changeable via pair element changes. The subtrees of rose trees
            would just be elements of a sequence, which can be changed like a
            sequence can be changed.

          – If a type is isomorphic to another type, we can just take the Change
            type of this other type and implement ($$) using the ($$) of this
            other type plus forward and backward application. For example,
            BinTree el is isomorphic to () `Either` (BinTree el, BinTree el).

      • We should do QuickCheck tests that test all the properties like
        commutativity of Trans–Change diagrams and monoid laws.

      • The state describes properties of the input data of the transformation.
        So it might be good to not just call it “state”, but something more
        precise. And we should have a commuting diagram property also for the
        “state”: generating the state via init and then modifying it via prop
        should result in the same as transforming the value via init and then
        generating the state for the resulting value via init. Is there some
        nice categorical structure hidden here?

      • Instead of introducing the Primitive data type, we should probably
        provide default implementations for the members of the Changeable class
        that are based on PrimitiveChange.

      • Sequence changes should also allow the elements to be changed. This can
        be achieved by adding the following data constructor to SeqChangeBase:

            Map :: (el -> el) -> SeqChangeBase (Seq el) (Seq el)

        The map function should be generalized such that it has the type

            (el ->> el') -> (Seq el ->> Seq el')

        instead of the current

            (el -> el') -> (Seq el ->> Seq el')  .

        Also the filter function should get a more general type, namely

            (el ->> Bool) -> (Seq el ->> Seq el)  .

        This means that booleans have to be changeable. Changes could be
        conjunctions and disjunctions, or maybe just the identity and
        overwrites. Even in the latter case, making booleans changeable is
        important for allowing transformations to Bool.

      • When propagating changes, we have to transform state according to the
        changes. In the case of filtering, for example, we have to split the
        list of booleans when a SplitAt is encountered. This transformation of
        state should be implemented generically for Cartesian.

      • Change propagation should be implemented generically for Cartesian as
        well.

      • We should come up with a type analogous to Cartesian that is about
        symmetric monoidal categories. That way, we could use the same base type
        in a linear and in a nonlinear setting. The only condition would be that
        the base type allows only for linear operations.

      • Is there a standard categorical structure that captures those changes
        that can be defined via Cartesian?

      • We could give the paper the title “Purely Functional Incremental
        Computation”. This sounds nicely propagandistic.

      • Maybe we should come up with a general operator that takes an initial
        value and a Traversable structure of changes and yields a Traversable
        structure of values resulting from applying these changes, and another
        one that yields a Traversable structure of propagated changes. Then we
        could work with lists of changes, but maybe also with event streams of
        changes. Maybe it is also enough to do this for lists; maybe we can
        reconstruct this thing for event streams from the operator for lists.

      • We should use the Newtype class or a similar class to define the
        conversion from an ordinary data type like [el] into a representation
        with a single shape layer like ListShape [el]. We can then wrap and
        unwrap on demand. We can also implement a fold operator based on this
        approach. If this operator does not just work for ->, but for any
        category, it gives us an incrementalizable fold.
-}
