module Data.Incremental.Tuple (

    {-NOTE:
        We would have liked to re-export (,), like we re-export Seq from
        Data.Incremental.Sequence. However, we could not find a way to
        re-export (,).
    -}

    -- * Changes

    first,
    second,

    -- * Atomic changes

    AtomicChange (First, Second),

    -- * Transformations

    (&&&),
    fst,
    snd,
    swap

) where

-- Prelude

import           Prelude hiding (fst, snd)
import qualified Prelude

-- Data

import           Data.Monoid (Monoid (mempty, mappend))
import qualified Data.Tuple as Tuple
import           Data.MultiChange (MultiChange)
import qualified Data.MultiChange as MultiChange
import           Data.Incremental

-- * Changes

instance (Changeable a, Changeable b) => Changeable (a, b) where

    type StdChange (a, b) = MultiChange (AtomicChange a b)

first :: StdChange a -> StdChange (a, b)
first = MultiChange.singleton . First

second :: StdChange b -> StdChange (a, b)
second = MultiChange.singleton . Second

-- * Atomic changes

data AtomicChange a b = First (StdChange a) | Second (StdChange b)

instance (Changeable a, Changeable b) => Change (AtomicChange a b) where

    type Value (AtomicChange a b) = (a, b)

    First change  $$ (val1, val2) = (change $$ val1, val2)
    Second change $$ (val1, val2) = (val1, change $$ val2)

-- * Transformations

(&&&) :: (Changeable a, Changeable b, Changeable c) =>
         (a ->> b) -> (a ->> c) -> (a ->> (b, c))
trans1 &&& trans2 = stTrans (\ val -> do
    ~(val1, prop1) <- toSTProc trans1 val
    ~(val2, prop2) <- toSTProc trans2 val
    let prop change = do
            change1 <- prop1 change
            change2 <- prop2 change
            return (first change1 `mappend` second change2)
    return ((val1, val2), prop))

fst :: (Changeable a, Changeable b) => (a, b) ->> a
fst = MultiChange.composeMap $ simpleTrans Prelude.fst prop where

    prop (First change) = change
    prop (Second _)     = mempty

snd :: (Changeable a, Changeable b) => (a, b) ->> b
snd = MultiChange.composeMap $ simpleTrans Prelude.snd prop where

    prop (First _)       = mempty
    prop (Second change) = change

swap :: (Changeable a, Changeable b) => (a, b) ->> (b, a)
swap = MultiChange.map $ simpleTrans Tuple.swap prop where

    prop (First change)  = Second change
    prop (Second change) = First change
