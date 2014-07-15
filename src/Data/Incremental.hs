module Data.Incremental (
    -- FIXME: Fix the export list.
    testResult
) where

import Prelude hiding (id, (.), splitAt, map, concat, concatMap, filter)

import           Control.Category
import           Control.Arrow
import           Data.Monoid
import           Data.Foldable    hiding (concat, concatMap)
import           Data.Sequence    hiding (filter)
import qualified Data.Sequence    as Seq
import           Data.FingerTree  as FingerTree

infixr 0 $$
infixr 0 ==>
infixr 7 :*:

-- * Core

class Monoid (Change val) => Changeable val where

    type Change val :: *

    -- NOTE: Operator $$ is at least not used in the base library.
    ($$) :: Change val -> val -> val

-- NOTE: Operator ==> is at least not used in the base library.
data val ==> val' where

    Trans :: (val -> (val',state))
          -> (Change val -> state -> (Change val',state))
          -> (val ==> val')

instance Category (==>) where

    id = Trans (\ val -> (val,())) (,)

    Trans init2 prop2 . Trans init1 prop1 = Trans init prop where

        init val = (val'',(state1,state2)) where

            (val',state1)  = init1 val

            (val'',state2) = init2 val'

        prop change (state1,state2) = (change'',(state1',state2')) where

            (change',state1')  = prop1 change state1

            (change'',state2') = prop2 change' state2

-- * Primitive changeables

newtype Primitive val = Primitive val

data PrimitiveChange val = Identity | Replace val

instance Monoid (PrimitiveChange val) where

    mempty = Identity

    Identity      `mappend` change1 = change1
    Replace val'' `mappend` _       = Replace val''

instance Changeable (Primitive val) where

    type Change (Primitive val) = PrimitiveChange val

    Identity     $$ Primitive val = Primitive val
    Replace val' $$ _             = Primitive val'

-- * Tuple structures

data TupleStruct = Data | Null | TupleStruct :*: TupleStruct

{-FIXME:
    Change this GADT to a data family with newtypes, so that we have no runtime
    overhead. I guess that the type index does not need to be fixed by pattern
    matching on values of Tuple, but is already fixed otherwise (Cartesian is a
    GADT, in particular).
-}
data Tuple unit pair el struct where

    ElementTuple :: { toElement :: el }
                 -> Tuple unit pair el Data

    UnitTuple    :: { toUnit :: unit }
                 -> Tuple unit pair el Null

    PairTuple    :: { toPair :: pair (Tuple unit pair el struct1)
                                     (Tuple unit pair el struct2) }
                 -> Tuple unit pair el (struct1 :*: struct2)

-- * Cartesian changes

data Cartesian base i o where

    Base    :: base i o
            -> Cartesian base i o

    Id      :: Cartesian base struct struct

    (:.:)   :: Cartesian base struct' struct''
            -> Cartesian base struct struct'
            -> Cartesian base struct struct''

    (:&&&:) :: Cartesian base i o1
            -> Cartesian base i o2
            -> Cartesian base i (o1 :*: o2)

    Fst     :: Cartesian base (struct1 :*: struct2) struct1

    Snd     :: Cartesian base (struct1 :*: struct2) struct2

    Drop    :: Cartesian base dummy Null

instance Category (Cartesian base) where

    id = Id

    (.) = (:.:)

type OrdinaryTuple = Tuple () (,)

toArrow :: Arrow arrow
        => (forall i o .
            base i o -> arrow (OrdinaryTuple el i) (OrdinaryTuple el o))
        -> Cartesian base i o -> arrow (OrdinaryTuple el i) (OrdinaryTuple el o)
toArrow fromBase (Base base)         = fromBase base
toArrow _        Id                  = id
toArrow fromBase (cart2 :.: cart1)   = toArrow fromBase cart2 .
                                       toArrow fromBase cart1
toArrow fromBase (cart1 :&&&: cart2) = toArrow fromBase cart1 &&&
                                       toArrow fromBase cart2 >>>
                                       arr PairTuple
toArrow _        Fst                 = arr (toPair >>> fst)
toArrow _        Snd                 = arr (toPair >>> snd)
toArrow _        Drop                = arr (const (UnitTuple ()))

type CartesianChange base = Cartesian base Data Data

instance Monoid (CartesianChange base) where

    mempty = id

    mappend = (.)

-- * Sequences

data SeqChangeBase el i o where

    SplitAt :: Int ->    SeqChangeBase el Data            (Data :*: Data)

    Cat     ::           SeqChangeBase el (Data :*: Data) Data

    Const   :: Seq el -> SeqChangeBase el Null            Data

seqChangeBaseToFun :: SeqChangeBase el i o
                   -> OrdinaryTuple (Seq el) i
                   -> OrdinaryTuple (Seq el) o
seqChangeBaseToFun (SplitAt idx) = toElement                     >>>
                                   splitAt idx                   >>>
                                   ElementTuple *** ElementTuple >>>
                                   PairTuple
seqChangeBaseToFun Cat           = toPair                  >>>
                                   toElement *** toElement >>>
                                   uncurry (Seq.><)        >>>
                                   ElementTuple
seqChangeBaseToFun (Const seq)   = const seq    >>>
                                   ElementTuple

instance Changeable (Seq el) where

    type Change (Seq el) = CartesianChange (SeqChangeBase el)

    ($$) change = ElementTuple                      >>>
                  toArrow seqChangeBaseToFun change >>>
                  toElement

-- * Mapping

mapSeqChangeMorph :: (el -> el')
                  -> Cartesian (SeqChangeBase el)  i o
                  -> Cartesian (SeqChangeBase el') i o
mapSeqChangeMorph _   (Base (SplitAt idx))    = Base (SplitAt idx)
mapSeqChangeMorph _   (Base Cat)              = Base Cat
mapSeqChangeMorph fun (Base (Const seq))      = Base (Const (fmap fun seq))
mapSeqChangeMorph _   Id                      = Id
mapSeqChangeMorph fun (change2 :.: change1)   = mapSeqChangeMorph fun change2 :.:
                                                mapSeqChangeMorph fun change1
mapSeqChangeMorph fun (change1 :&&&: change2) = mapSeqChangeMorph fun change1 :&&&:
                                                mapSeqChangeMorph fun change2
mapSeqChangeMorph _   Fst                     = Fst
mapSeqChangeMorph _   Snd                     = Snd
mapSeqChangeMorph _   Drop                    = Drop
-- FIXME: Width.

map :: (el -> el') -> Seq el ==> Seq el'
map fun = Trans init prop where

    init seq = (fmap fun seq, ())

    prop change _ = (mapSeqChangeMorph fun change, ())

-- * Concatenation

concatSeq :: Seq (Seq el) -> Seq el
concatSeq = asum

newtype ConcatStateElement = ConcatStateElement Int

data ConcatStateMeasure = ConcatStateMeasure {
                              sourceLength :: Int,
                              targetLength :: Int
                          }

instance Monoid ConcatStateMeasure where

    mempty = ConcatStateMeasure 0 0

    mappend (ConcatStateMeasure srcLength1 tgtLength1)
            (ConcatStateMeasure srcLength2 tgtLength2) = measure' where
            
        measure' = ConcatStateMeasure (srcLength1 + srcLength2)
                                      (tgtLength1 + tgtLength2)

instance Measured ConcatStateMeasure ConcatStateElement where

    measure (ConcatStateElement elLength) = ConcatStateMeasure 1 elLength

type ConcatState = FingerTree ConcatStateMeasure ConcatStateElement

seqToConcatState :: Seq (Seq el) -> ConcatState
seqToConcatState = toList                                 >>>
                   fmap (ConcatStateElement . Seq.length) >>>
                   FingerTree.fromList

splitConcatStateAt :: Int -> ConcatState -> (ConcatState,ConcatState)
splitConcatStateAt idx = split ((> idx) . sourceLength)

concatSeqChangeMorph :: Cartesian (SeqChangeBase (Seq el)) i o
                     -> OrdinaryTuple ConcatState i
                     -> (Cartesian (SeqChangeBase el) i o,OrdinaryTuple ConcatState o)
concatSeqChangeMorph (Base (SplitAt idx))    (ElementTuple state)                                  = let

                                                                                                         (state1,state2) = splitConcatStateAt idx state

                                                                                                     in (Base (SplitAt (targetLength (measure state1))),
                                                                                                         PairTuple (ElementTuple state1,ElementTuple state2))
concatSeqChangeMorph (Base Cat)              (PairTuple (ElementTuple state1,ElementTuple state2)) = (Base Cat,ElementTuple (state1 FingerTree.>< state2))
concatSeqChangeMorph (Base (Const seq))      _                                                     = (Base (Const (concatSeq seq)),ElementTuple (seqToConcatState seq))
concatSeqChangeMorph Id                      state                                                 = (Id,state)
concatSeqChangeMorph (change2 :.: change1)   state                                                 = let

                                                                                                         (change1',state')  = concatSeqChangeMorph change1 state

                                                                                                         (change2',state'') = concatSeqChangeMorph change2 state'

                                                                                                     in (change2' :.: change1',state'')
concatSeqChangeMorph (change1 :&&&: change2) state                                                 = let

                                                                                                         (change1',state1) = concatSeqChangeMorph change1 state

                                                                                                         (change2',state2) = concatSeqChangeMorph change2 state

                                                                                                     in (change1' :&&&: change2',PairTuple (state1,state2))
concatSeqChangeMorph Fst                     (PairTuple (state1,_))                                = (Fst,state1)
concatSeqChangeMorph Snd                     (PairTuple (_,state2))                                = (Snd,state2)
concatSeqChangeMorph Drop                    _                                                     = (Drop,UnitTuple ())
-- FIXME: Width and layout.
-- FIXME: Choose a different identifier, since we also track the state here, contrary to map.
-- FIXME: state1 and state2 are not (necessarily) ConcatState values.

concat :: Seq (Seq el) ==> Seq el
concat = Trans init prop where

    init seq = (concatSeq seq, seqToConcatState seq)

    prop change state = second toElement $
                        concatSeqChangeMorph change (ElementTuple state)

-- * Monadic structure

-- FIXME: Add return.

concatMap :: (el -> Seq el') -> Seq el ==> Seq el'
concatMap f = concat . map f

-- * Filtering

filter :: (el -> Bool) -> Seq el ==> Seq el
filter prd = concatMap (\ el -> if prd el then Seq.singleton el else Seq.empty)

-- * Example

initialSeq :: Seq Integer
initialSeq = Seq.fromList [2,3,5,7,11,13,17,19]

bimap :: Cartesian base i1 o1
      -> Cartesian base i2 o2
      -> Cartesian base (i1 :*: i2) (o1 :*: o2)
bimap cart1 cart2 = (Fst >>> cart1) :&&&: (Snd >>> cart2)

swap :: Cartesian base (struct1 :*: struct2) (struct2 :*: struct1)
swap = Snd :&&&: Fst

newSubseq :: Seq Integer
newSubseq = Seq.fromList [10,103]

change :: Change (Seq Integer)
change = Base (SplitAt 4)                                      >>> -- ([2,3,5,7],[11,13,17,19])
         bimap id (Base (SplitAt 2))                           >>> -- ([2,3,5,7],([11,13],[17,19]))
         bimap id Snd                                          >>> -- ([2,3,5,7],[17,19])
         swap                                                  >>> -- ([17,19],[2,3,5,7])
         bimap (id :&&&: (Drop >>> Base (Const newSubseq))) id >>> -- (([17,19],[10,103]),[2,3,5,7])
         bimap (Base Cat) id                                   >>> -- ([17,19,10,103],[2,3,5,7])
         (Base Cat)                                                -- [17,19,10,103,2,3,5,7]

trans :: Seq Integer ==> Seq Integer
trans = filter (\ num -> num `mod` 10 /= 3) >>> -- [2,5,7,11,17,19] / [17,19,10,2,5,7]
        map succ                            >>> -- [3,6,8,12,18,20] / [18,20,11,3,6,8]
        filter (\ num -> num `mod` 4 == 0)      -- [8,12,20]        / [20,8]

testResult :: Seq Integer
testResult = case trans of

                 Trans init prop -> let

                                        (val',state) = init initialSeq

                                        (change',_) = prop change state

                                    in change' $$ val'

{-FIXME:
    The following things are to be considered:

      • Implement maps using finger trees:

          – Subsection 4.7 of the finger trees paper already explains the
            essential things. We should reference this subsection in our paper.

          – We should implement all the operations of Data.Map, including the
            generic mergeWithKey function, but we should implement union in
            terms of mergeWithKey only if this results in the efficient merging
            algorithm shown in the finger trees paper.

          – We should take splitting at a key and union as the primitive map
            changes.

          – We should call our module Data.Map.FingerTree, in line with
            Data.IntervalMap.FingerTree and Data.PriorityQueue.FingerTree from
            the fingertree package.

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
            BinTree el is isomorphic to () `Either` (BinTree el,BinTree el).

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

            (el ==> el') -> (Seq el ==> Seq el')

        instead of the current

            (el -> el') -> (Seq el ==> Seq el')  .

        Also the filter function should get a more general type, namely

            (el ==> Bool) -> (Seq el ==> Seq el)  .

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
