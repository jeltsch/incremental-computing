module Data.Incremental (

    -- * Transformations

    type (->>),
    UniversalSimpleOpsConv,
    UniversalInfoOpsConv,
    SimpleOpsConv (SimpleOpsConv),
    InfoOpsConv (InfoOpsConv),
    AbstractOps (AbstractOps),
    simpleTrans,
    infoTrans,
    toPropagator,

    -- * Propagators

    Propagator,
    WithTransConts,
    TransCont,

    -- * Generators

    type Generator (Generator),
    type Modifier (Modifier),
    generatorMap,
    modifierMap,

    -- * Operations

    type Ops (Ops, pack, unpack, coreOps),
    unitOps,
    zipOps,
    stdOps,
    dynamicInfoOps,

    -- * Core operations

    UnitInternal,
    ZipInternals,
    CoreOperations (
        DataOf,
        canonicalCoreOps,
        unitCoreOps,
        zipCoreOps,
        StdInternal,
        stdCoreOps
    ),
    coreOpsEq,

    -- * Individual operations

    -- ** Constructors

    Constructor (Constructor, runConstructor),
    wholeConstructor,
    unitConstructor,
    zipConstructors,
    (<:>),
    constructorMap,
    infoConstructorMap,
    deepConstructorLift,
    shallowConstructorLift,
    jointInfoConstructor,

    -- ** Editors

    Editor (Editor, runEditor),
    wholeEditor,
    unitEditor,
    zipEditors,
    editorMap,
    infoEditorMap,
    deepEditorLift,
    shallowEditorLift,
    jointInfoEditor,
    withInputInfo,

    -- * Data

    Data (CanonicalCoreOps, coreOpsEqFromCanonicity)

) where

-- Control

import Control.Arrow (first, second, (***))
import Control.Monad.Trans.Writer (WriterT (WriterT), runWriterT)
import Control.Monad.Trans.State (StateT (StateT), runStateT)

-- Data

import Data.Kind (Type)
import Data.Type.Equality ((:~~:))
import Data.Tuple (swap)

-- GHC

import GHC.Exts (Constraint)

-- Fixities

infixl 9 <:>

-- * Transformations

newtype a ->> b = Trans (Propagator Generator a b)

{-NOTE:
    Type application patterns make the introduction of aux in simpleTrans,
    simpleTransCont, infoTrans, and infoTransCont unnecessary. The type o' can
    be acquired via the pattern for the input generator, and then opsConv can be
    applied to it. Once we do not have aux anymore, we can integrate the
    applications of withSimpleTransConts and withInfoTransConts into the case
    expressions, getting rid of the current extra passes via generatorMap and
    modifierMap and consequently also of the WriterT wrapping of the f (e, q)
    result in infoTrans and infoTransCont.
-}

type UniversalSimpleOpsConv a b = (forall j' (o' :: j' -> Type -> Type -> Type) .
                                   (CoreOperations o', DataOf o' ~ a) =>
                                   SimpleOpsConv b o')

type UniversalInfoOpsConv q a b = (forall j' (o' :: j' -> Type -> Type -> Type) .
                                   (CoreOperations o', DataOf o' ~ a) =>
                                   InfoOpsConv q b o')

data SimpleOpsConv b o' where
    SimpleOpsConv :: CoreOperations o
                  => (forall e . AbstractOps o e -> AbstractOps o' e)
                  -> SimpleOpsConv (DataOf o) o'

data InfoOpsConv q b o' where
    InfoOpsConv :: CoreOperations o
                => (forall e . AbstractOps o e -> AbstractOps o' (e, q))
                -> InfoOpsConv q (DataOf o) o'

data AbstractOps o e = forall i p . AbstractOps (Ops o i p e)

simpleTrans :: UniversalSimpleOpsConv a b -> (a ->> b)
simpleTrans opsConv = Trans $ \ (Generator genFun') ->
                      generatorMap (withSimpleTransConts opsConv) $
                      aux opsConv genFun'

    where

    aux :: Functor f
        => SimpleOpsConv b o'
        -> (forall i' p' e' . Ops o' i' p' e' -> f e')
        -> Generator b f
    aux (SimpleOpsConv opsConvFun) genFun'
        = Generator $ \ ops ->
          case opsConvFun (AbstractOps ops) of
              AbstractOps ops' -> genFun' ops'

simpleTransCont :: UniversalSimpleOpsConv a b -> TransCont a b
simpleTransCont opsConv = TransCont $ \ (Modifier modFun') ->
                          modifierMap (withSimpleTransConts opsConv) $
                          aux opsConv modFun'

    where

    aux :: Functor f
        => SimpleOpsConv b o'
        -> (forall i' p' e' . Ops o' i' p' e' -> e' -> f e')
        -> Modifier b f
    aux (SimpleOpsConv opsConvFun) modFun'
        = Modifier $ \ ops ->
          case opsConvFun (AbstractOps ops) of
              AbstractOps ops' -> modFun' ops'

withSimpleTransConts :: Functor f
                     => UniversalSimpleOpsConv a b
                     -> f e
                     -> WithTransConts a b f e
withSimpleTransConts opsConv output
    = WriterT $ (, simpleTransCont opsConv) <$> output

infoTrans :: UniversalInfoOpsConv q a b -> (a ->> b)
infoTrans opsConv = Trans $ \ (Generator genFun') ->
                    generatorMap (withInfoTransConts opsConv) $
                    aux opsConv genFun'

    where

    aux :: Functor f
        => InfoOpsConv q b o'
        -> (forall i' p' e' . Ops o' i' p' e' -> f e')
        -> Generator b (WriterT q f)
    aux (InfoOpsConv opsConvFun) genFun'
        = Generator $ \ ops ->
          case opsConvFun (AbstractOps ops) of
              AbstractOps ops' -> WriterT $ genFun' ops'

infoTransCont :: UniversalInfoOpsConv q a b -> q -> TransCont a b
infoTransCont opsConv info = TransCont $ \ (Modifier modFun') ->
                             modifierMap (withInfoTransConts opsConv) $
                             aux opsConv info modFun'

    where

    aux :: Functor f
        => InfoOpsConv q b o'
        -> q
        -> (forall i' p' e' . Ops o' i' p' e' -> e' -> f e')
        -> Modifier b (WriterT q f)
    aux (InfoOpsConv opsConvFun) info modFun'
        = Modifier $ \ ops ->
          case opsConvFun (AbstractOps ops) of
              AbstractOps ops' -> WriterT . flip (curry (modFun' ops')) info

withInfoTransConts :: Functor f
                   => UniversalInfoOpsConv q a b
                   -> WriterT q f e
                   -> WithTransConts a b f e
withInfoTransConts opsConv output
    = WriterT $ second (infoTransCont opsConv) <$> runWriterT output

toPropagator :: (a ->> b) -> Propagator Generator a b
toPropagator (Trans prop) = prop

-- * Propagators

type Propagator h a b = forall f . Functor f =>
                        h a f -> h b (WithTransConts a b f)

type WithTransConts a b f = WriterT (TransCont a b) f

newtype TransCont a b = TransCont (Propagator Modifier a b)

-- * Generators and modifiers

data Generator a f where

    Generator :: CoreOperations o
              => (forall i p e . Ops o i p e -> f e)
              -> Generator (DataOf o) f

data Modifier a f where

    Modifier :: CoreOperations o
             => (forall i p e . Ops o i p e -> e -> f e)
             -> Modifier (DataOf o) f

generatorMap :: (forall e . f e -> g e) -> Generator a f -> Generator a g
generatorMap fun (Generator genFun) = Generator (fun . genFun)

modifierMap :: (forall e . f e -> g e) -> Modifier a f -> Modifier a g
modifierMap fun (Modifier modFun) = Modifier ((fun .) . modFun)

-- * Operations

data Ops o i p e = Ops {
    pack    :: e -> p,
    unpack  :: p -> e,
    coreOps :: o i p e
}

unitOps :: CoreOperations o
        => Ops o UnitInternal () ()
unitOps = Ops {
    pack    = id,
    unpack  = id,
    coreOps = unitCoreOps
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

dynamicInfoOps :: (e -> p)
               -> (p -> e)
               -> o i (p, q) (e, q)
               -> Ops o i (p, q) (e, q)
dynamicInfoOps pack unpack infoCoreOps = Ops {
    pack    = first pack,
    unpack  = first unpack,
    coreOps = infoCoreOps
}

-- * Core operations

type family UnitInternal :: j

type family ZipInternals (i1 :: j) (i2 :: j) :: j

class Data (DataOf o) => CoreOperations (o :: j -> Type -> Type -> Type) where

    type DataOf o :: Type

    canonicalCoreOps :: CanonicalCoreOps (DataOf o) o

    unitCoreOps :: o UnitInternal () ()

    zipCoreOps :: o i1 p1 e1
               -> o i2 p2 e2
               -> o (ZipInternals i1 i2) (p1, p2) (e1, e2)

    type StdInternal o :: j

    stdCoreOps :: o (StdInternal o) (DataOf o) (DataOf o)

coreOpsEq :: (CoreOperations o1, CoreOperations o2, DataOf o1 ~ DataOf o2)
          => o1 :~~: o2
coreOpsEq = coreOpsEqFromCanonicity canonicalCoreOps canonicalCoreOps

-- * Individual operations

-- ** Constructors

newtype Constructor o i p d = Constructor {
    runConstructor :: forall f . Functor f =>
                      (forall e . Ops o i p e -> f e) -> f d
}

wholeConstructor :: Ops o i p e
                 -> Constructor o i p e
wholeConstructor ops = Constructor $ \ newArgs -> newArgs ops

unitConstructor :: CoreOperations o
                => Constructor o UnitInternal () ()
unitConstructor = wholeConstructor unitOps

zipConstructors :: CoreOperations o
                => Constructor o i1 p1 d1
                -> Constructor o i2 p2 d2
                -> Constructor o (ZipInternals i1 i2) (p1, p2) (d1, d2)
zipConstructors (Constructor construct1) (Constructor construct2)
    = Constructor $ \ newArgs -> runWriterT $
                                 writerTExchange $
                                 construct2 $ \ argOps2 ->
                                 writerTExchange $
                                 construct1 $ \ argOps1 ->
                                 WriterT $
                                 newArgs (zipOps argOps1 argOps2)

{-NOTE:
    The nullary analog of (<:>) is (<$), which allows for writing
    unitConstructor <$ ... <$ unitConstructor.
-}

-- NOTE: This allows for writing (zipConstructors <:> ... <:> zipConstructors).
(<:>) :: Functor f
      => (c -> d -> e)
      -> (a -> b -> f (c, d))
      -> (a -> b -> f e)
(fun <:> funcFun) val1 val2 = uncurry fun <$> funcFun val1 val2

instance Functor (Constructor o i p) where

    fmap to constructor = Constructor $ \ newArgs ->
                          to <$>
                          constructor `runConstructor` newArgs

    entity <$ constructor = Constructor $ \ newArgs ->
                            entity <$
                            constructor `runConstructor` newArgs

constructorMap :: (d -> d')
               -> Constructor o i p d
               -> Constructor o i p d'
constructorMap = fmap

infoConstructorMap :: (q -> q')
                   -> Constructor o i p (d, q)
                   -> Constructor o i p (d, q')
infoConstructorMap to = constructorMap $ second to

deepConstructorLift :: (forall e . Ops o i p e -> Ops o' i' p' (e, q))
                    -> Constructor o i p d
                    -> Constructor o' i' p' (d, q)
deepConstructorLift opsConv constructor
    = Constructor $ \ newArgs' ->
      runWriterT $
      constructor `runConstructor` (\ ops -> WriterT $ newArgs' (opsConv ops))

shallowConstructorLift :: q
                       -> Constructor o i p d
                       -> Constructor o i p (d, q)
shallowConstructorLift info = constructorMap $ (, info)

jointInfoConstructor :: ((q, c) -> q')
                     -> Constructor o i p ((d, q), c)
                     -> Constructor o i p (d, q')
jointInfoConstructor to = constructorMap $ second to . rightAssoc

-- ** Editors

newtype Editor o i p d = Editor {
    runEditor :: forall f r . Functor f =>
                 (forall e . Ops o i p e -> StateT e f r) -> StateT d f r
}

wholeEditor :: Ops o i p e
            -> Editor o i p e
wholeEditor ops = Editor $ \ procPart -> procPart ops

unitEditor :: CoreOperations o
           => Editor o UnitInternal () ()
unitEditor = wholeEditor unitOps

zipEditors :: CoreOperations o
           => Editor o i1 p1 d1
           -> Editor o i2 p2 d2
           -> Editor o (ZipInternals i1 i2) (p1, p2) (d1, d2)
zipEditors (Editor edit1) (Editor edit2)
    = Editor $ \ procPart -> stateTUncurry $
                             stateTFlip $
                             edit2 $ \ partOps2 ->
                             stateTFlip $
                             edit1 $ \ partOps1 ->
                             stateTCurry $
                             procPart (zipOps partOps1 partOps2)

editorMap :: (d' -> d)
          -> (d -> d')
          -> Editor o i p d
          -> Editor o i p d'
editorMap from to editor
    = Editor $ \ procPart ->
      StateT $ \ entity' ->
      second to <$>
      (editor `runEditor` procPart) `runStateT` from entity'

infoEditorMap :: (q' -> q)
              -> (q -> q')
              -> Editor o i p (d, q)
              -> Editor o i p (d, q')
infoEditorMap from to = editorMap (second from) (second to)

deepEditorLift :: (forall e . Ops o i p e -> Ops o' i' p' (e, q))
               -> Editor o i p d
               -> Editor o' i' p' (d, q)
deepEditorLift opsConv editor
    = Editor $ \ procPart' ->
      stateTUncurry $
      editor `runEditor` (\ ops -> stateTCurry $ procPart' (opsConv ops))

shallowEditorLift :: Editor o i p d
                  -> Editor o i p (d, q)
shallowEditorLift editor
    = Editor $ \ procPart ->
      stateTUncurry $
      stateTFlip $
      stateTLift $
      editor `runEditor` procPart

jointInfoEditor :: (q' -> (q, c))
                -> ((q, c) -> q')
                -> Editor o i p ((d, q), c)
                -> Editor o i p (d, q')
jointInfoEditor from to = editorMap (leftAssoc . second from)
                                    (second to . rightAssoc)

withInputInfo :: (q -> Editor o i p (d, q))
              -> Editor o i p (d, q)
withInputInfo fun = Editor $ \ procPart ->
                    StateT $ \ entityAndInfo@(_, info) ->
                    (fun info `runEditor` procPart) `runStateT` entityAndInfo

-- * Utilities

-- This has a more general type than Control.Monad.Trans.Class.lift.
stateTLift :: Functor f
           => f a
           -> StateT s f a
stateTLift comp = StateT $ \ state -> (, state) <$> comp

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

writerTExchange :: Functor f
                => WriterT w f a
                -> WriterT a f w
writerTExchange (WriterT comp) = WriterT $ swap <$> comp

leftAssoc :: (a, (b, c)) -> ((a, b), c)
leftAssoc (val1, (val2, val3)) = ((val1, val2), val3)

rightAssoc :: ((a, b), c) -> (a, (b, c))
rightAssoc ((val1, val2), val3) = (val1, (val2, val3))

-- * Data

class Data (a :: Type) where

    data CanonicalCoreOps a :: (j -> Type -> Type -> Type) -> Type

    coreOpsEqFromCanonicity :: CanonicalCoreOps a o1
                            -> CanonicalCoreOps a o2
                            -> o1 :~~: o2
