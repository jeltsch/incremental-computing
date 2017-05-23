module Data.Incremental (

    -- * Data

    type Data (CoreOperations, StdCoreOps, stdCoreOps),

    -- * Operations

    type Ops (Ops, pack, unpack, coreOps),
    stdOps,
    LensOp,

    -- * Transformations

    type (->>) (Trans),
    type Generator

) where

-- Control

import Control.Monad.Trans.State

-- GHC

import GHC.Exts (Constraint)

-- * Data

class CoreOperations a (StdCoreOps a) => Data a where

    type CoreOperations a (o :: * -> * -> *) :: Constraint

    type StdCoreOps a :: * -> * -> *

    stdCoreOps :: StdCoreOps a a a

-- * Operations

data Ops o p d = Ops {
    pack    :: d -> p,
    unpack  :: p -> d,
    coreOps :: o p d
}

stdOps :: Data a => Ops (StdCoreOps a) a a
stdOps = Ops {
    pack    = id,
    unpack  = id,
    coreOps = stdCoreOps
}

type LensOp subCoreOps _sub dat = forall r .
                                  (forall sub . Ops subCoreOps _sub sub ->
                                                State sub r) ->
                                  State dat r

-- * Transformations

newtype a ->> b = Trans (forall f . Functor f => Generator a f -> Generator b f)

type Generator a f = forall o p d . CoreOperations a o => Ops o p d -> f d
