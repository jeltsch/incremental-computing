module Data.Incremental (

    -- * Data

    Data (CanonicalCoreOps, coreOpsEqFromCan),

    -- * Core operations

    ZipInternals,
    CoreOperations (
        DataOf,
        canonicalCoreOps,
        zipCoreOps,
        StdInternal,
        stdCoreOps
    ),
    coreOpsEq,

    -- * Operations

    type Ops (Ops, pack, unpack, coreOps),
    zipOps,
    stdOps,
    convOps,
    dynInfoOpsConv,

    -- * Individual operations

    Constructor (Constructor),
    zipConstructors,
    Editor (Editor),
    zipEditors,

    -- * Generators

    type Generator (Generator),

    -- * Transformations

    type (->>) (Trans),
    infoTransCore

) where

-- Control

import Control.Arrow
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State

-- Data

import Data.Kind (Type)
import Data.Type.Equality
import           Data.Tuple

-- GHC

import GHC.Exts (Constraint)

-- * Data

class Data a where

    data CanonicalCoreOps a :: (j -> Type -> Type -> Type) -> Type

    coreOpsEqFromCan :: CanonicalCoreOps a o1
                     -> CanonicalCoreOps a o2
                     -> o1 :~~: o2

-- * Core operations

type family ZipInternals (i1 :: j) (i2 :: j) :: j

class Data (DataOf o) => CoreOperations (o :: j -> Type -> Type -> Type) where

    type DataOf o :: Type

    canonicalCoreOps :: CanonicalCoreOps (DataOf o) o

    zipCoreOps :: o i1 p1 e1
               -> o i2 p2 e2
               -> o (ZipInternals i1 i2) (p1, p2) (e1, e2)

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

zipOps :: CoreOperations o
       => Ops o i1 p1 e1
       -> Ops o i2 p2 e2
       -> Ops o (ZipInternals i1 i2) (p1, p2) (e1, e2)
zipOps (Ops pack1 unpack1 coreOps1) (Ops pack2 unpack2 coreOps2) = Ops {
    pack    = pack1 *** pack2,
    unpack  = unpack1 *** unpack2,
    coreOps = zipCoreOps coreOps1 coreOps2
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

newtype Constructor o i p e = Constructor (forall f . Functor f =>
                                           (forall e' . Ops o i p e' -> f e') ->
                                           f e)

instance Functor (Constructor o i p) where

    fmap fun (Constructor construct) = Constructor $
                                       \ newArgs -> fmap fun (construct newArgs)

    val <$ Constructor construct = Constructor $
                                   \ newArgs -> val <$ construct newArgs

zipConstructors :: CoreOperations o
                => Constructor o i1 p1 e1
                -> Constructor o i2 p2 e2
                -> Constructor o (ZipInternals i1 i2) (p1, p2) (e1, e2)
zipConstructors (Constructor construct1) (Constructor construct2)
    = Constructor $ \ newArgs -> runWriterT $
                                 writerTExchange $
                                 construct2 $ \ argOps2 ->
                                 writerTExchange $
                                 construct1 $ \ argOps1 ->
                                 WriterT $
                                 newArgs (zipOps argOps1 argOps2)

{-FIXME:
    Change Monad to Functor once transformations are implemented using
    zipCoreOps.
-}
newtype Editor o i p e = Editor (forall m r . Monad m =>
                                 (forall e' . Ops o i p e' -> StateT e' m r) ->
                                 StateT e m r)

zipEditors :: CoreOperations o
           => Editor o i1 p1 e1
           -> Editor o i2 p2 e2
           -> Editor o (ZipInternals i1 i2) (p1, p2) (e1, e2)
zipEditors (Editor edit1) (Editor edit2)
    = Editor $ \ procPart -> stateTUncurry $
                             stateTFlip $
                             edit2 $ \ partOps2 ->
                             stateTFlip $
                             edit1 $ \ partOps1 ->
                             stateTCurry $
                             procPart (zipOps partOps1 partOps2)

-- * Monad transformer utilities

stateTCurry :: Functor f
            => StateT (s1, s2) f a
            -> StateT s1 (StateT s2 f) a
stateTCurry comp = StateT $ \ state1 -> StateT $ \ state2 ->
                   leftAssoc <$> comp `runStateT` (state1, state2)

stateTUncurry :: Functor f
              => StateT s1 (StateT s2 f) a
              -> StateT (s1, s2) f a
stateTUncurry comp = StateT $ \ (state1, state2) ->
                     rightAssoc <$> (comp `runStateT` state1) `runStateT` state2

stateTFlip :: Functor f
           => StateT s1 (StateT s2 f) a
           -> StateT s2 (StateT s1 f) a
stateTFlip comp = StateT $ \ state2 ->
                  StateT $ \ state1 ->
                  leftAssoc . second swap . rightAssoc <$>
                  (comp `runStateT` state1) `runStateT` state2

leftAssoc :: (a, (b, c)) -> ((a, b), c)
leftAssoc (val1, (val2, val3)) = ((val1, val2), val3)

rightAssoc :: ((a, b), c) -> (a, (b, c))
rightAssoc ((val1, val2), val3) = (val1, (val2, val3))

writerTExchange :: Functor f
                => WriterT w f a
                -> WriterT a f w
writerTExchange (WriterT comp) = WriterT $ swap <$> comp

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
