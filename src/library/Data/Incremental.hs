module Data.Incremental (

    -- * Data

    type Data (CoreOperations, StdCoreOps, stdCoreOps),

    -- * Operations

    type Ops (Ops, pack, unpack, coreOps),
    stdOps,
    convOps,
    mapCoreOps,
    dynInfoOpsConv,
    ArgMaker,
    LensOp,

    -- * Transformations

    type (->>) (Trans),
    type Generator,
    infoTrans

) where

-- Control

import Control.Arrow (first)
import Control.Monad.Trans.State

-- GHC

import GHC.Exts (Constraint)

-- * Data

class CoreOperations a (StdCoreOps a) => Data a where

    type CoreOperations a (o :: * -> * -> *) :: Constraint

    type StdCoreOps a :: * -> * -> *

    stdCoreOps :: StdCoreOps a a a

-- * Operations

data Ops o p e = Ops {
    pack    :: e -> p,
    unpack  :: p -> e,
    coreOps :: o p e
}

stdOps :: Data a => Ops (StdCoreOps a) a a
stdOps = Ops {
    pack    = id,
    unpack  = id,
    coreOps = stdCoreOps
}

convOps :: ((e1 -> p1) -> (e2 -> p2))
        -> ((p1 -> e1) -> (p2 -> e2))
        -> (Ops o1 p1 e1 -> o2 p2 e2)
        -> Ops o1 p1 e1
        -> Ops o2 p2 e2
convOps packFun unpackFun coreOpsFun ops@(Ops { .. }) = Ops {
    pack    = packFun pack,
    unpack  = unpackFun unpack,
    coreOps = coreOpsFun ops
}

mapCoreOps :: (o1 p e -> o2 p e) -> Ops o1 p e -> Ops o2 p e
mapCoreOps fun = convOps id id (fun . coreOps)

dynInfoOpsConv :: (Ops o1 p e -> o2 (p, i) (e, i))
               -> Ops o1 p e
               -> Ops o2 (p, i) (e, i)
dynInfoOpsConv = convOps first first

type ArgMaker o p = forall e . Ops o p e -> e

type LensOp o p e' = forall r .
                     (forall e . Ops o p e -> State e r) -> State e' r

-- * Transformations

newtype a ->> b = Trans (forall f . Functor f => Generator a f -> Generator b f)

type Generator a f = forall o p e . CoreOperations a o => Ops o p e -> f e

infoTrans :: forall i a b .
             (forall f .
                  Functor f =>
                  Generator a f ->
                  (forall o p e . CoreOperations b o => Ops o p e -> f (e, i)))
          -> (a ->> b)
infoTrans conv = Trans (\ gen -> fmap fst . conv gen)

{-NOTE:
    An alternative version of infoTrans is as follows:

        infoTrans :: forall i a b .
                     (forall f .
                          Functor f =>
                          (forall o p e . CoreOperations a o => Ops o p (e, i) -> f e) ->
                          Generator b f)
                  -> (a ->> b)
        infoTrans conv = Trans (\ gen -> conv (fmap fst . gen))

    Compared to the version above, it uses f e instead of f (e, i) as the result
    type of the continuations, and consequently, it drops the info before the
    continuation conversion instead of afterwards.
-}
