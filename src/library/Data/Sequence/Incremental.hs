module Data.Sequence.Incremental (

    -- * Type

    type Seq,

    -- * Operations

    type SeqOps (SeqOps, empty, singleton, onSlice, onElement)

    -- * Transformations

) where

import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- * Type

instance Data a => Data (Seq a) where

    type Specific (Seq a) u = forall elemOps _elem . Operations a elemOps =>
                              u (SeqOps elemOps _elem)

-- * Operations

data SeqOps elemOps _elem _seq seq = SeqOps {
    empty     :: seq,
    singleton :: (forall elem . AllOps elemOps _elem elem -> elem)
              -> seq,
    onSlice   :: forall r .
                 Int
              -> Int
              -> (forall seq' . AllOps (SeqOps elemOps _elem) _seq seq' ->
                                State seq' r)
              -> State seq r,
    onElement :: forall r .
                 Int
              -> (forall elem . AllOps elemOps _elem elem ->
                                State elem r)
              -> State seq r
}

instance Operations a elemOps => Operations (Seq a) (SeqOps elemOps _elem) where

    generalize = id

-- * Transformations
