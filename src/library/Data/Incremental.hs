module Data.Incremental (

    -- * Data

    type Data (Specific),

    -- * Operations

    type AllOps (AllOps, pack, unpack, ops),
    type Operations (Dat, generalize),
    type OpsCont (OpsCont),

    -- * Transformations

    type (->>) (Trans),
    type Generator

) where

-- * Data

class Data a where

    type Specific a (u :: (* -> * -> *) -> *)

-- * Operations

data AllOps o p i = AllOps {
    pack   :: i -> p,
    unpack :: p -> i,
    ops    :: o p i
}

class Operations o where

    type Dat o

    generalize :: Specific (Dat o) u -> u o

newtype OpsCont r o = OpsCont (forall p i . o p i -> r)

-- * Transformations

data a ->> b = Trans (forall r . Generator a r -> Generator b r)

type Generator a r = forall o p i . (Operations o, Dat o ~ a) =>
                     AllOps o p i -> r
