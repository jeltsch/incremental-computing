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
import Control.Arrow (second)
import Control.Monad (liftM)

-- Data

import           Data.Monoid
import           Data.List (foldl')
import           Data.DList (DList)
import qualified Data.DList as DList
import           Data.Incremental

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
map trans = stTrans (liftM (second liftProp) . toSTProc trans) where

    liftProp prop = liftM fromList . mapM prop . toList

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
