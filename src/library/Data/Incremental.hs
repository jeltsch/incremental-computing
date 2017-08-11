module Data.Incremental (

    -- * Data

    Data (CanonicalCoreOps, coreOpsEqFromCan),

    -- * Core operations

    CoreOperations (DataOf, canonicalCoreOps, StdInternal, stdCoreOps),
    coreOpsEq,

    -- * Operations

    type Ops (Ops, pack, unpack, coreOps),
    stdOps,
    convOps,
    dynInfoOpsConv,
    ArgMaker,
    LensOp,

    -- * Generators

    type Generator (Generator),

    -- * Transformations

    type (->>) (Trans),
    infoTransCore

) where

-- Control

import Control.Arrow (first)
import Control.Monad.Trans.State

-- Data

import Data.Kind (Type)
import Data.Type.Equality

-- GHC

import GHC.Exts (Constraint)

-- * Data

class Data a where

    data CanonicalCoreOps a :: (j -> Type -> Type -> Type) -> Type

    coreOpsEqFromCan :: CanonicalCoreOps a o1
                     -> CanonicalCoreOps a o2
                     -> o1 :~~: o2

-- * Core operations

class Data (DataOf o) => CoreOperations (o :: j -> Type -> Type -> Type) where

    type DataOf o :: Type

    canonicalCoreOps :: CanonicalCoreOps (DataOf o) o

    type StdInternal o :: j

    stdCoreOps :: o (StdInternal o) (DataOf o) (DataOf o)

coreOpsEq :: (CoreOperations o1, CoreOperations o2, DataOf o1 ~ DataOf o2)
          => o1 :~~: o2
coreOpsEq = coreOpsEqFromCan canonicalCoreOps canonicalCoreOps

-- * Operations

data Ops o i p e = Ops {
    pack    :: e -> p,
    unpack  :: p -> e,
    coreOps :: o i p e
}

stdOps :: CoreOperations o => Ops o (StdInternal o) (DataOf o) (DataOf o)
stdOps = Ops {
    pack    = id,
    unpack  = id,
    coreOps = stdCoreOps
}

convOps :: ((e1 -> p1) -> (e2 -> p2))
        -> ((p1 -> e1) -> (p2 -> e2))
        -> (Ops o1 i1 p1 e1 -> o2 i2 p2 e2)
        -> Ops o1 i1 p1 e1
        -> Ops o2 i2 p2 e2
convOps packFun unpackFun coreOpsFun ops@(Ops { .. }) = Ops {
    pack    = packFun pack,
    unpack  = unpackFun unpack,
    coreOps = coreOpsFun ops
}

dynInfoOpsConv :: (Ops o1 i1 p e -> o2 i2 (p, i) (e, i))
               -> Ops o1 i1 p e
               -> Ops o2 i2 (p, i) (e, i)
dynInfoOpsConv = convOps first first

-- * Individual operations

type ArgMaker o i p = forall e . Ops o i p e -> e

type LensOp o i p e' = forall r .
                       (forall e . Ops o i p e -> State e r) -> State e' r

-- * Generators

data Generator a f where

    Generator :: CoreOperations o
              => (forall i p e . Ops o i p e -> f e)
              -> Generator (DataOf o) f

-- * Transformations

newtype a ->> b = Trans (forall f . Functor f => Generator a f -> Generator b f)

infoTransCore :: (CoreOperations o2, Functor f)
              => ((forall i p e . Ops o1 i p e -> f e) ->
                  (forall i p e . Ops o2 i p e -> f (e, q)))
              -> (forall i p e . Ops o1 i p e -> f e)
              -> Generator (DataOf o2) f
infoTransCore genFunConv genFun = Generator $ fmap fst . genFunConv genFun
