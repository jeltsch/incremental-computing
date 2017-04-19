module Data.Incremental (

    -- * Data

    type Data (CoreOperations, StdCoreOps, stdCoreOps),

    -- * Operations

    type Ops (Ops, pack, unpack, coreOps),
    stdOps,

    -- * Transformations

    type (->>) (Trans),
    type Generator

) where

-- GHC

import GHC.Exts (Constraint)

-- * Data

class CoreOperations a (StdCoreOps a) => Data a where

    type CoreOperations a (o :: * -> * -> *) :: Constraint

    type StdCoreOps a :: * -> * -> *

    stdCoreOps :: StdCoreOps a a a

-- * Operations

data Ops o p i = Ops {
    pack    :: i -> p,
    unpack  :: p -> i,
    coreOps :: o p i
}

stdOps :: Data a => Ops (StdCoreOps a) a a
stdOps = Ops {
    pack    = id,
    unpack  = id,
    coreOps = stdCoreOps
}

-- * Transformations

newtype a ->> b = Trans (forall f . Functor f => Generator a f -> Generator b f)

type Generator a f = forall o p i . CoreOperations a o => Ops o p i -> f i
