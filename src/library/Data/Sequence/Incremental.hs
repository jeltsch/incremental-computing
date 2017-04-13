module Data.Sequence.Incremental (

    -- * Type

    type Seq,

    -- * Operations

    type SeqOps (SeqOps, empty, singleton, onSlice, onElement)

    -- * Transformations

) where

-- Control

import Control.Monad.Trans.State

-- Data

import           Data.Incremental
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- * Type

instance Data a => Data (Seq a) where

    newtype Specific (Seq a) u = SeqSpecific (
                forall elemOps _elem . (Operations elemOps, Dat elemOps ~ a) =>
                u (SeqOps elemOps _elem)
            )

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

instance Operations elemOps => Operations (SeqOps elemOps _elem) where

    type Dat (SeqOps elemOps _) = Seq (Dat elemOps)

    generalize (SeqSpecific ops) = ops

-- * Transformations
