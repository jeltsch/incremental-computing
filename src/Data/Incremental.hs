module Data.Incremental (
    -- FIXME: Fill the export list.
) where

import Prelude hiding (id, (.), splitAt)

import Control.Category
import Control.Arrow
import Data.Monoid
import Data.Sequence

infixr 0 $$
infixr 0 ==>

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

data PrimitiveChange val = Identity | Overwrite val

instance Monoid (PrimitiveChange val) where

    mempty = Identity

    Identity        `mappend` change1 = change1
    Overwrite val'' `mappend` _       = Overwrite val''

instance Changeable (Primitive val) where

    type Change (Primitive val) = PrimitiveChange val

    Identity       $$ Primitive val = Primitive val
    Overwrite val' $$ _             = Primitive val'

-- * Sequences
instance Changeable (Seq el) where

    type Change (Seq el) = SeqChange el

    change $$ seq = toArrow seqChangeBaseToFun change $ seq

type SeqChange el = CartesianChange (SeqChangeBase el) (Seq el)

data SeqChangeBase el i o where

    SplitAt :: Int ->    SeqChangeBase el (Seq el)        (Seq el,Seq el)

    Cat     ::           SeqChangeBase el (Seq el,Seq el) (Seq el)

    Const   :: Seq el -> SeqChangeBase el ()              (Seq el)

seqChangeBaseToFun :: SeqChangeBase el i o -> i -> o
seqChangeBaseToFun (SplitAt idx) = splitAt idx
seqChangeBaseToFun Cat           = uncurry (><)
seqChangeBaseToFun (Const seq)   = const seq

-- * Cartesian changes
type CartesianChange base val = Cartesian base val val

instance Monoid (CartesianChange base val) where

    mempty = id

    mappend = (.)

data Cartesian base i o where

    Base    :: base i o
            -> Cartesian base i o

    Id      :: Cartesian base val val

    (:.:)   :: Cartesian base val' val''
            -> Cartesian base val val'
            -> Cartesian base val val''

    (:&&&:) :: Cartesian base val val1
            -> Cartesian base val val2
            -> Cartesian base val (val1,val2)

    Fst     :: Cartesian base (val1,val2) val1

    Snd     :: Cartesian base (val1,val2) val2

    Drop    :: Cartesian base dummy ()

instance Category (Cartesian base) where

    id = Id

    (.) = (:.:)

toArrow :: Arrow arrow => (forall i o . base i o -> arrow i o)
                       -> Cartesian base i o -> arrow i o
toArrow fromBase (Base base)         = fromBase base
toArrow _        Id                  = id
toArrow fromBase (cart2 :.: cart1)   = toArrow fromBase cart2 .
                                       toArrow fromBase cart1
toArrow fromBase (cart1 :&&&: cart2) = toArrow fromBase cart1 &&&
                                       toArrow fromBase cart2
toArrow _        Fst                 = arr fst
toArrow _        Snd                 = arr snd
toArrow _        Drop                = arr (const ())

{-FIXME:
    The following things are to be considered:

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

      • Maybe we should not use types of kind * as parameters of Cartesian and
        base types, but instead types of an algebraic data kind that uses a
        special marker for the value type (Seq el, or whatever). That way, it
        might be easier to track states, because in the case of state tracking,
        we work with a different core type (state of the change instead of Seq
        el).

      • Is there a standard categorical structure that captures those changes
        that can be defined via Cartesian?
-}
