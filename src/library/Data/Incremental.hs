module Data.Incremental (

    -- * Transformations

    type (->>) (Trans),
    PreTrans,
    TransCore (TransCore),
    preTrans,
    InfoTransCore (InfoTransCore),
    infoPreTrans,

    -- * Generators

    type Generator (Generator),

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
    ConstructorLifting (ConstructorLifting),
    deepConstructorLift,
    shallowConstructorLift,
    constructorMap,
    jointInfoConstructor,
    deepInfoConstructorLift,
    shallowInfoConstructorLift,
    infoConstructorMap,

    -- ** Editors

    Editor (Editor, runEditor),
    wholeEditor,
    unitEditor,
    zipEditors,
    EditorLifting (EditorLifting),
    deepEditorLift,
    shallowEditorLift,
    editorMap,
    withInput,
    jointInfoEditor,
    deepInfoEditorLift,
    shallowInfoEditorLift,
    infoEditorMap,
    withInputInfo,

    -- * Data

    Data (CanonicalCoreOps, coreOpsEqFromCanonicity)

) where

-- Control

import Control.Arrow
import Control.Monad.Fix
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State

-- Data

import Data.Kind (Type)
import Data.Type.Equality
import Data.Tuple

-- GHC

import GHC.Exts (Constraint)

-- Fixities

infixl 9 <:>

-- * Transformations

newtype a ->> b = Trans (forall f . Functor f => Generator a f -> Generator b f)

type PreTrans o o' f = (forall i' p' e' . Ops o' i' p' e' -> f e') ->
                       Generator (DataOf o) f

data TransCore o o' i p e
    = forall i' p' e' . TransCore (Ops o i p e -> Ops o' i' p' e') (e' -> e)

preTrans :: (CoreOperations o, Functor f)
         => (forall i p e . TransCore o o' i p e)
         -> PreTrans o o' f
preTrans transCore genFun'
    = Generator $
      case transCore of
          TransCore opsConv entityConv -> fmap entityConv . genFun' . opsConv

data InfoTransCore o o' i p e
    = forall i' p' q . InfoTransCore (Ops o i p e -> Ops o' i' p' (e, q))

infoPreTrans :: (CoreOperations o, Functor f)
             => (forall i p e . InfoTransCore o o' i p e)
             -> PreTrans o o' f
infoPreTrans infoTransCore = preTrans $
                             case infoTransCore of
                                 InfoTransCore opsConv -> TransCore opsConv fst

-- * Generators

data Generator a f where

    Generator :: CoreOperations o
              => (forall i p e . Ops o i p e -> f e)
              -> Generator (DataOf o) f

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

data ConstructorLifting o o' i i' p p' d d' e
    = forall e' . ConstructorLifting (Ops o i p e -> Ops o' i' p' e')
                                     (e' -> (e, d -> d'))

deepConstructorLift :: (forall e . ConstructorLifting o o' i i' p p' d d' e)
                    -> Constructor o i p d
                    -> Constructor o' i' p' d'
deepConstructorLift lifting constructor
    = Constructor $ \ newArgs' ->
      collapseOuterConstructorGadget $
      outerConstructorGadget lifting constructor newArgs'

type ConstructorGadget d d' f e = WriterT (d -> d') f e

collapseOuterConstructorGadget :: Functor f
                               => ConstructorGadget d d' f d
                               -> f d'
collapseOuterConstructorGadget = fmap (uncurry (flip ($))) . runWriterT

outerConstructorGadget :: Functor f
                       => (forall e . ConstructorLifting o o' i i' p p' d d' e)
                       -> Constructor o i p d
                       -> (forall e' . Ops o' i' p' e' -> f e')
                       -> ConstructorGadget d d' f d
outerConstructorGadget lifting (Constructor construct) newArgs'
    = construct $ \ ops ->
      case lifting of
          ConstructorLifting opsConv outputConvs
              -> innerConstructorGadget outputConvs (newArgs' (opsConv ops))

innerConstructorGadget :: Functor f
                       => (e' -> (e, d -> d'))
                       -> f e'
                       -> ConstructorGadget d d' f e
innerConstructorGadget outputConvs = WriterT . fmap outputConvs

shallowConstructorLift :: (d -> d')
                       -> Constructor o i p d
                       -> Constructor o i p d'
shallowConstructorLift entityConv constructor
    = Constructor $ \ newArgs ->
      entityConv <$>
      constructor `runConstructor` newArgs

constructorMap :: (d -> d')
               -> Constructor o i p d
               -> Constructor o i p d'
constructorMap = shallowConstructorLift

instance Functor (Constructor o i p) where

    fmap = constructorMap

    entity <$ constructor = Constructor $ \ newArgs ->
                            entity <$
                            constructor `runConstructor` newArgs

jointInfoConstructor :: ((q, c) -> q')
                     -> Constructor o i p ((d, q), c)
                     -> Constructor o i p (d, q')
jointInfoConstructor to = constructorMap $ second to . rightAssoc

deepInfoConstructorLift :: (forall e . Ops o i p e -> Ops o' i' p' (e, q))
                        -> Constructor o i p d
                        -> Constructor o' i' p' (d, q)
deepInfoConstructorLift opsConv = deepConstructorLift $
                                  ConstructorLifting opsConv detachInfo

shallowInfoConstructorLift :: q
                           -> Constructor o i p d
                           -> Constructor o i p (d, q)
shallowInfoConstructorLift info = shallowConstructorLift (, info)

infoConstructorMap :: (q -> q')
                   -> Constructor o i p (d, q)
                   -> Constructor o i p (d, q')
infoConstructorMap to = constructorMap $ second to

-- ** Editors

{-FIXME:
    Change MonadFix to FunctorFix once transformations are implemented using
    zipCoreOps (and remove the import of Control.Monad.Fix).
-}
newtype Editor o i p d = Editor {
    runEditor :: forall f r . MonadFix f =>
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

data EditorLifting o o' i i' p p' d d' e
    = forall e' . EditorLifting (Ops o i p e -> Ops o' i' p' e')
                                (d' -> (d, e -> e'))
                                (e' -> (e, d -> d'))

deepEditorLift :: (forall e . EditorLifting o o' i i' p p' d d' e)
               -> Editor o i p d
               -> Editor o' i' p' d'
deepEditorLift lifting editor = Editor $ \ procPart' ->
                                collapseOuterEditorGadget $
                                outerEditorGadget lifting editor procPart'

type EditorGadget d d' f r e = d' -> StateT e f (r, (d, d -> d'))

collapseOuterEditorGadget :: MonadFix f
                          => EditorGadget d d' f r d
                          -> StateT d' f r
collapseOuterEditorGadget gadget
    = StateT $ \ outerEntity' ->
      second (uncurry ($)) . rightAssoc . first (second snd) <$>
      mfix (runStateT (gadget outerEntity') . fst . snd . fst)

outerEditorGadget :: MonadFix f
                  => (forall e . EditorLifting o o' i i' p p' d d' e)
                  -> Editor o i p d
                  -> (forall e' . Ops o' i' p' e' -> StateT e' f r)
                  -> EditorGadget d d' f r d
outerEditorGadget lifting (Editor edit) procPart' outerEntity'
    = edit $ \ ops ->
      case lifting of
          EditorLifting opsConv inputConvs outputConvs
              -> innerEditorGadget inputConvs
                                   outputConvs
                                   (procPart' (opsConv ops))
                                   outerEntity'

innerEditorGadget :: Functor f
                  => (d' -> (d, e -> e'))
                  -> (e' -> (e, d -> d'))
                  -> StateT e' f r
                  -> EditorGadget d d' f r e
innerEditorGadget inputConvs outputConvs stateT' outerEntity'
    = StateT $ \ innerEntity ->
      leftAssoc . second (swap . second ((,) outerEntity) . outputConvs) <$>
      stateT' `runStateT` innerEntityConv innerEntity

    where

    (outerEntity, innerEntityConv) = inputConvs outerEntity'

shallowEditorLift :: (d' -> (d, d -> d'))
                  -> Editor o i p d
                  -> Editor o i p d'
shallowEditorLift convs editor = Editor $ \ procPart ->
                              StateT $ \ entity' ->
                              let (entity, entityConv) = convs entity' in
                              second entityConv <$>
                              (editor `runEditor` procPart) `runStateT` entity

editorMap :: (d' -> d)
          -> (d -> d')
          -> Editor o i p d
          -> Editor o i p d'
editorMap from to = shallowEditorLift ((, to) . from)

withInput :: (d -> Editor o i p d)
          -> Editor o i p d
withInput fun = Editor $ \ procPart ->
                StateT $ \ entity ->
                (fun entity `runEditor` procPart) `runStateT` entity

jointInfoEditor :: (q' -> (q, c))
                -> ((q, c) -> q')
                -> Editor o i p ((d, q), c)
                -> Editor o i p (d, q')
jointInfoEditor from to = editorMap (leftAssoc . second from)
                                    (second to . rightAssoc)

deepInfoEditorLift :: (forall e . Ops o i p e -> Ops o' i' p' (e, q))
                   -> Editor o i p d
                   -> Editor o' i' p' (d, q)
deepInfoEditorLift opsConv = deepEditorLift $
                             EditorLifting opsConv detachInfo detachInfo

shallowInfoEditorLift :: Editor o i p d
                      -> Editor o i p (d, q)
shallowInfoEditorLift = shallowEditorLift detachInfo

infoEditorMap :: (q' -> q)
              -> (q -> q')
              -> Editor o i p (d, q)
              -> Editor o i p (d, q')
infoEditorMap from to = editorMap (second from) (second to)

withInputInfo :: (q -> Editor o i p (d, q))
              -> Editor o i p (d, q)
withInputInfo fun = withInput $ fun . snd

-- * Utilities

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

detachInfo :: (e, q) -> (e, d -> (d, q))
detachInfo = fst &&& flip (,) . snd

-- * Data

class Data (a :: Type) where

    data CanonicalCoreOps a :: (j -> Type -> Type -> Type) -> Type

    coreOpsEqFromCanonicity :: CanonicalCoreOps a o1
                            -> CanonicalCoreOps a o2
                            -> o1 :~~: o2
