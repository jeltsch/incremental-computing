module Data.MultiChange (

    -- * Type

    MultiChange,

    -- * Construction

    singleton,
    fromList,

    -- * Deconstruction

    toList,

    -- * Monad structure

    map,
    return,
    join,
    bind

) where

-- Prelude

import Prelude hiding (id, (.), map, return)

-- Control

import Control.Category

-- Data

import           Data.Monoid
import           Data.List (foldl')
import           Data.DList (DList)
import qualified Data.DList as DList
import           Data.Incremental
import           Data.Incremental.Internal

-- * Type

newtype MultiChange p = MultiChange (Dual (DList p)) deriving Monoid

instance Change p => Change (MultiChange p) where

    type Value (MultiChange p) = Value p

    change $$ val = foldl' (flip ($$)) val (toList change)

-- * Construction

singleton :: p -> MultiChange p
singleton = MultiChange . Dual . DList.singleton

{-NOTE:
    The lists are “in diagramatic order” (first atomic change at the beginning).
-}

fromList :: [p] -> MultiChange p
fromList = MultiChange . Dual . DList.fromList

-- * Deconstruction

toList :: MultiChange p -> [p]
toList (MultiChange (Dual dList)) = DList.toList dList

-- * Monad structure

map :: Trans p q -> Trans (MultiChange p) (MultiChange q)
map (Trans conv) = Trans liftedConv where

    liftedConv ~(val, multiChanges) = (val', multiChanges') where

        changeLists = fmap toList multiChanges

        (val', changes') = conv (val, concat changeLists)

        multiChanges' = group (fmap length changeLists) changes'

    group :: [Int] -> [q] -> [MultiChange q]
    group (len : lens) changes = fromList headChanges :
                                 group lens tailChanges where

        (headChanges, tailChanges) = splitAt len changes

return :: Trans p (MultiChange p)
return = statelessTrans id singleton

join :: Trans (MultiChange (MultiChange p)) (MultiChange p)
join = statelessTrans id (mconcat . reverse . toList)
{-FIXME:
    Check whether the use of mconcat . reverse is questionable regarding space
    usage or strictness.
-}

bind :: Trans p (MultiChange q) -> Trans (MultiChange p) (MultiChange q)
bind trans = join . map trans
