module Data.Incremental (

    -- * Data

    type Data (Specific),

    -- * Operations

    type Ops (Ops, pack, unpack, coreOps),
    type CoreOps (DataOf, generalize),
    type OpsCont (OpsCont, unOpsCont),

    -- * Transformations

    type (->>) (Trans),
    type Generator

) where

-- * Data

class Data a where

    data Specific a (u :: (* -> * -> *) -> *)

-- * Operations

data Ops o p i = Ops {
    pack    :: i -> p,
    unpack  :: p -> i,
    coreOps :: o p i
}

class CoreOps o where

    type DataOf o

    generalize :: Specific (DataOf o) u -> u o

newtype OpsCont p i r o = OpsCont {
                              unOpsCont :: Ops o p i -> r
                          }

-- * Transformations

data a ->> b = Trans (forall r . Generator a r -> Generator b r)

type Generator a r = forall o p i . (CoreOps o, DataOf o ~ a) =>
                     Ops o p i -> r
