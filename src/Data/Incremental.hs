module Data.Incremental (
    -- FIXME: Fix the export list.
    testResult
) where

import Prelude hiding (id, (.), splitAt, map, concat, concatMap, filter)

import           Control.Category
import           Control.Arrow
import           Data.Monoid
import           Data.Foldable       hiding (concat, concatMap)
import           Data.Sequence       hiding (filter)
import qualified Data.Sequence       as Seq
import           Data.Map.FingerTree
import qualified Data.Map.FingerTree as Map
import           Data.FingerTree     as FingerTree

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

{- FIXME:
    Consider implementing a (&&&) and a const (or drop, that is, const ())
    for (==>).
-}

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
data Tuple u p el struct where

    E :: { unE :: el }
      -> Tuple u p el Data

    U :: { unU :: u }
      -> Tuple u p el Null

    P :: { unP :: Tuple u p el struct1 `p` Tuple u p el struct2 }
      -> Tuple u p el (struct1 :*: struct2)
{- FIXME:
    Mention in the documentation that E, U, and P stand for “element”, “unit”,
    and “pair”, respectively.
-}

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

eToP :: (val -> (val,val))
     -> OrdinaryTuple val Data
     -> OrdinaryTuple val (Data :*: Data)
eToP fun = unE >>> fun >>> E *** E >>> P

pToE :: (val -> val -> val)
     -> OrdinaryTuple val (Data :*: Data)
     -> OrdinaryTuple val Data
pToE fun = unP >>> unE *** unE >>> uncurry fun >>> E

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
                                       arr P
toArrow _        Fst                 = arr (unP >>> fst)
toArrow _        Snd                 = arr (unP >>> snd)
toArrow _        Drop                = arr (const (U ()))

type CartesianChange base = Cartesian base Data Data

instance Monoid (CartesianChange base) where

    mempty = id

    mappend = (.)

-- * Sequences

data SeqChangeBase el i o where

    SplitAt :: Int ->    SeqChangeBase el Data            (Data :*: Data)

    Cat     ::           SeqChangeBase el (Data :*: Data) Data

    GenSeq  :: Seq el -> SeqChangeBase el Null            Data
-- FIXME: Maybe use Seq.Gen and Map.Gen instead of GenSeq and GenMap.

seqChangeBaseToFun :: SeqChangeBase el i o
                   -> OrdinaryTuple (Seq el) i
                   -> OrdinaryTuple (Seq el) o
seqChangeBaseToFun (SplitAt idx) = eToP (splitAt idx)
seqChangeBaseToFun Cat           = pToE (Seq.><)
seqChangeBaseToFun (GenSeq seq)  = const seq >>> E

instance Changeable (Seq el) where

    type Change (Seq el) = CartesianChange (SeqChangeBase el)

    ($$) change = E >>> toArrow seqChangeBaseToFun change >>> unE

-- * Mapping

mapSeqChangeMorph :: (el -> el')
                  -> Cartesian (SeqChangeBase el)  i o
                  -> Cartesian (SeqChangeBase el') i o
mapSeqChangeMorph _   (Base (SplitAt idx))    = Base (SplitAt idx)
mapSeqChangeMorph _   (Base Cat)              = Base Cat
mapSeqChangeMorph fun (Base (GenSeq seq))     = Base (GenSeq (fmap fun seq))
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
concatSeqChangeMorph (Base (SplitAt idx))    (E state)                                  = let

                                                                                              (state1,state2) = splitConcatStateAt idx state

                                                                                          in (Base (SplitAt (targetLength (measure state1))),P (E state1,E state2))
concatSeqChangeMorph (Base Cat)              (P (E state1,E state2))                    = (Base Cat,E (state1 FingerTree.>< state2))
concatSeqChangeMorph (Base (GenSeq seq))   _                                            = (Base (GenSeq (concatSeq seq)),E (seqToConcatState seq))
concatSeqChangeMorph Id                      state                                      = (Id,state)
concatSeqChangeMorph (change2 :.: change1)   state                                      = let

                                                                                              (change1',state')  = concatSeqChangeMorph change1 state

                                                                                              (change2',state'') = concatSeqChangeMorph change2 state'

                                                                                          in (change2' :.: change1',state'')
concatSeqChangeMorph (change1 :&&&: change2) state                                      = let

                                                                                              (change1',state1) = concatSeqChangeMorph change1 state

                                                                                              (change2',state2) = concatSeqChangeMorph change2 state

                                                                                          in (change1' :&&&: change2',P (state1,state2))
concatSeqChangeMorph Fst                     (P (state1,_))                             = (Fst,state1)
concatSeqChangeMorph Snd                     (P (_,state2))                             = (Snd,state2)
concatSeqChangeMorph Drop                    _                                          = (Drop,U ())
-- FIXME: Width and layout.
-- FIXME: Choose a different identifier, since we also track the state here, contrary to map.
-- FIXME: state1 and state2 are not (necessarily) ConcatState values.

concat :: Seq (Seq el) ==> Seq el
concat = Trans init prop where

    init seq = (concatSeq seq, seqToConcatState seq)

    prop change state = second unE $ concatSeqChangeMorph change (E state)

-- * Monadic structure

-- FIXME: Add return.

concatMap :: (el -> Seq el') -> Seq el ==> Seq el'
concatMap f = concat . map f

-- * Filtering

filter :: (el -> Bool) -> Seq el ==> Seq el
filter prd = concatMap (\ el -> if prd el then Seq.singleton el else Seq.empty)

-- * Maps

data MapChangeBase k a i o where

    SplitLeft  :: Ord k => k ->       MapChangeBase k a Data (Data :*: Data)

    SplitRight :: Ord k => k ->       MapChangeBase k a Data (Data :*: Data)

    Union      :: Ord k =>            MapChangeBase k a (Data :*: Data) Data

    GenMap     ::          Map k a -> MapChangeBase k a Null Data

splitBeside :: Ord k
            => (k -> Map k a -> Maybe a -> Map k a)
            -> (k -> Map k a -> Maybe a -> Map k a)
            -> k
            -> Map k a
            -> (Map k a,Map k a)
splitBeside hdl1 hdl2 splitKey map = (hdl1 splitKey map1 maybeSplitVal,
                                      hdl2 splitKey map2 maybeSplitVal) where

    (map1,maybeSplitVal,map2) = splitLookup splitKey map

incorporate :: Ord k => k -> Map k a -> Maybe a -> Map k a
incorporate _        map Nothing    = map
incorporate splitKey map (Just val) = insert splitKey val map

doNotIncorporate :: k -> Map k a -> Maybe a -> Map k a
doNotIncorporate = const const

splitLeft :: Ord k => k -> Map k a -> (Map k a,Map k a)
splitLeft = splitBeside doNotIncorporate incorporate

splitRight :: Ord k => k -> Map k a -> (Map k a,Map k a)
splitRight = splitBeside incorporate doNotIncorporate

mapChangeBaseToFun :: MapChangeBase k a i o
                   -> OrdinaryTuple (Map k a) i
                   -> OrdinaryTuple (Map k a) o
mapChangeBaseToFun (SplitLeft splitKey)  = eToP (splitLeft splitKey)
mapChangeBaseToFun (SplitRight splitKey) = eToP (splitRight splitKey)
mapChangeBaseToFun Union                 = pToE union
mapChangeBaseToFun (GenMap map)          = const map >>> E

instance Changeable (Map k a) where

    type Change (Map k a) = CartesianChange (MapChangeBase k a)

    ($$) change = E >>> toArrow mapChangeBaseToFun change >>> unE

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
change = Base (SplitAt 4)                                       >>> -- ([2,3,5,7],[11,13,17,19])
         bimap id (Base (SplitAt 2))                            >>> -- ([2,3,5,7],([11,13],[17,19]))
         bimap id Snd                                           >>> -- ([2,3,5,7],[17,19])
         swap                                                   >>> -- ([17,19],[2,3,5,7])
         bimap (id :&&&: (Drop >>> Base (GenSeq newSubseq))) id >>> -- (([17,19],[10,103]),[2,3,5,7])
         bimap (Base Cat) id                                    >>> -- ([17,19,10,103],[2,3,5,7])
         (Base Cat)                                                 -- [17,19,10,103,2,3,5,7]

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

      • Make Data.Map.FingerTree.Map an instance of Changeable, where the basic
        changes are splitLookup and union.

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

          – Generate value changes for those keys that need changes. Maybe, we
            can use the implementation of the filter transformation for this,
            where the predicate is equality with the respective key.

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
