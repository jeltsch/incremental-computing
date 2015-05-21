module Data.Incremental (

    -- * Changes

    Change (Value, ($$)),
    PrimitiveChange (Keep, ReplaceBy),

    -- * Transformations

    Trans,
    TransProc,

    -- ** Construction

    simpleTrans,
    stateTrans,
    stateTrans',
    stTrans,
    trans,

    -- ** Deconstruction

    runTrans,
    toFunction,
    toSTProc,

    -- ** Utilities

    const,
    fromFunction,
    sanitize,

    -- * Changeables

    Changeable (DefaultChange),
    type (->>)

) where

-- Prelude

import           Prelude hiding (id, (.), const)
import qualified Prelude

-- Control

import Control.Category
import Control.Monad.ST.Lazy
import Control.Monad.ST.Lazy.Unsafe

-- Data

import Data.Monoid
import Data.Functor.Identity
import Data.STRef.Lazy

infixr 0 $$
infixr 0 ->>

{-NOTE:
    Our policy regarding class constraints with Change and Changeable is as
    follows:

      • Global values that are about changes directly and do not use ($$) (which
        are almost all of them) should not have Change constraints. Adding all
        these change constraints everywhere would give us nothing and only
        introduce clutter and possibly performance issues.

      • Global values that are about changeables (which first and foremost
        includes all that are about (->>)) should have Changeable constraints,
        because this ensures that standard changes are monoids and the value
        type of standard changes is the type that we started with.
-}

-- * Changes

class Change p where

    type Value p :: *

    -- NOTE: Operator $$ is at least not used in the base library.
    ($$) :: p -> Value p -> Value p

data PrimitiveChange a = Keep | ReplaceBy a deriving (Show, Read)

instance Functor PrimitiveChange where

    fmap _   Keep            = Keep
    fmap fun (ReplaceBy val) = ReplaceBy (fun val)

instance Monoid (PrimitiveChange a) where

    mempty = Keep

    Keep          `mappend` change = change
    ReplaceBy val `mappend` _      = ReplaceBy val

instance Change (PrimitiveChange a) where

    type Value (PrimitiveChange a) = a

    Keep          $$ val = val
    ReplaceBy val $$ _   = val

-- * Transformations

newtype Trans p q = Trans ((Value p, [p]) -> (Value q, [q]))

instance Category Trans where

    id = Trans id

    Trans conv2 . Trans conv1 = Trans (conv2 . conv1)

type TransProc m p q = Value p -> m (Value q, p -> m q)

-- ** Construction

simpleTrans :: (Value p -> Value q) -> (p -> q) -> Trans p q
simpleTrans fun prop = trans (\ cont -> runIdentity (cont transProc)) where

    transProc val = return (fun val, return . prop)

stateTrans :: (Value p -> (Value q, s)) -> (p -> s -> (q, s)) -> Trans p q
stateTrans init prop = stTrans (\ val -> do
    let (val', initState) = init val
    stateRef <- newSTRef initState
    let stProp change = do
            oldState <- readSTRef stateRef
            let (change', newState) = prop change oldState
            writeSTRef stateRef newState
            return change'
    return (val', stProp))

{-FIXME:
    Say in the documentation that stateTrans' is state-strict in the sense that
    reduction of the initial target value and any target change will result in
    reduction of the state. Point out that reduction is only to WHNF, so that
    the initializer and propagator have to make sure that WHNF reduction
    triggers more reduction (for example, by using a data type with strict
    fields) if this is desired.
-}
stateTrans' :: (Value p -> (Value q, s)) -> (p -> s -> (q, s)) -> Trans p q
stateTrans' init prop = stateTrans init' prop' where

    init' val = (initState `seq` val', initState) where

        (val', initState) = init val

    prop' change oldState = (newState `seq` change', newState) where

        (change', newState) = prop change oldState

{-FIXME:
    Say in the documentation that it is the resposibility of the user of stTrans
    to make sure that reductions of the initial target and target changes
    trigger evaluation of any parts of the state for which this is desired.
-}
stTrans :: (forall s . TransProc (ST s) p q) -> Trans p q
stTrans transProc = trans (\ cont -> runST (cont transProc))

{-NOTE:
    ST with OrderT layers around can be run as follows:

        transNested :: (forall o1 ... on s .
                        TransProc (OrderT o1 (... (OrderT on (ST s)))) p q)
                    -> Trans p q
        transNested transProc = trans (\ cont -> runST (
                                                 evalOrderT (
                                                 ... (
                                                 evalOrderT (cont transProc)))))
-}

{-FIXME:
    We have to mention in the documentation that the monad is supposed to be
    lazy. If it is strict, the constructed transformation trans will (probably)
    have the following properties:

      • Reducing any expression runTrans trans src to WHNF results in the
        initialization being run and the constructed propagator being run on all
        the changes.

      • The expression toSTProc trans is a processor that always yields ⊥ as the
        output value and constructs propagators that always yield ⊥ as the
        output change.
-}
{-FIXME:
    Say in the documentation that what holds for stTrans regarding state
    strictness also holds for trans.
-}
trans :: (forall r . (forall m . Monad m => TransProc m p q -> m r) -> r)
      -> Trans p q
trans cpsProcAndRun = errorIfStrictMonad `seq` Trans conv where

    errorIfStrictMonad = cpsProcAndRun $
                         Prelude.const (strictMonadError >> return ())

    strictMonadError = error "Data.Incremental: \
                             \Transformation processor uses strict monad"

    conv src = cpsProcAndRun $ \ transProc -> monadicConv transProc src

    monadicConv transProc ~(val, changes) = do
        ~(val', prop) <- transProc val
        changes' <- mapM prop changes
        return (val', changes')

-- ** Deconstruction

runTrans :: Trans p q -> (Value p, [p]) -> (Value q, [q])
runTrans (Trans conv) = conv

toFunction :: Trans p q -> (Value p -> Value q)
toFunction (Trans conv) val = fst (conv (val, undefined))

{-FIXME:
    We have to mention the following in the documentation:

        The function toSTProc . stTrans is not the identity. A computation in
        the original value of type forall s . TransProc (ST s) may yield an
        undefined state, but for computations in the constructed value,
        undefinedness can only occur in the values they output.

        On the other hand, stTrans . toSTProc is the identity. [At least, it
        should be.]
-}
{-FIXME:
    It is crucial that toSTProc cannot be called on functions of type

        (Value p, [p]) -> (Value q, [q])  ,

    but only on transformations, which correspond only to sensible, in
    particular causal, functions.

    Take, for example, the following function:

        \ ~(val, ~(change1 : ~(change2 : rest))) -> (val, change2 : change1 : rest)

    (Maybe, we do not even need to use lazy patterns.) If we would apply a
    function like toSTProc to it, and apply runTrans to the result, we would get
    a function that is not referentially transparent. Let this function be
    called f. Let us proceed as follows:

        let input    = (False, [ReplaceBy False, ReplaceBy True])
        let output   = f input
        let changes' = snd output
        let change1' = changes' !! 0
        let change2' = changes' !! 1

    If we now evaluate change1', we will hit ⊥, because the second input change
    has not been written into the channel. However, if we first evaluate
    change2' and then change1', then change1' will evaluate to ReplaceBy True.

    This particular problem should not occur with our toSTProc, which only works
    with transformations. If a user would reimplement toSTProc such that it
    works with arbitrary functions of the above-mentioned type, he would have to
    use unsafeInterleaveST directly, where there would be no guarantees anyhow.

    That said, we have to analyze very carefully whether our toSTProc is really
    completely safe. Only if it is, we should declare a module that contains it
    trustworthy (in the sense of Safe Haskell). We have to take into account
    that trans works with arbitrary runnable monad families and an instantiation
    of the Monad class could be bogus. The argument that running a
    transformation always yields causal functions relies on the assumption that
    the output of the first argument of (>>=) cannot depend on data that is only
    contained in the second argument of (>>=). Maybe, this assumption can be
    broken with a bogus Monad instance. But maybe, parametricity ensures that
    this assumption holds.
-}

toSTProc :: Trans p q -> TransProc (ST s) p q
toSTProc (Trans conv) val = do
    (chan, changes) <- newChannel
    let (val', changes') = conv (val, changes)
    remainderRef <- newSTRef changes'
    let prop change = do
            writeChannel chan change
            next : further <- readSTRef remainderRef
            writeSTRef remainderRef further
            return next
    return (val', prop)

-- ** Utilities

const :: Monoid q => Value q -> Trans p q
const val = simpleTrans (Prelude.const val) (Prelude.const mempty)

fromFunction :: (a -> b) -> Trans (PrimitiveChange a) (PrimitiveChange b)
fromFunction fun = simpleTrans fun (fmap fun)

sanitize :: Eq a => Trans (PrimitiveChange a) (PrimitiveChange a)
sanitize = stateTrans init prop where

    init val = (val, val)

    prop Keep            state = (Keep, state)
    prop (ReplaceBy val) state = if val == state
                                     then (Keep, state)
                                     else (ReplaceBy val, val)

-- * Changeables

class (Monoid (DefaultChange a),
       Change (DefaultChange a),
       Value (DefaultChange a) ~ a) =>
      Changeable a where

    type DefaultChange a :: *
    type DefaultChange a = PrimitiveChange a

instance Changeable Bool

instance Changeable Int

{-FIXME:
    Add default instance declarations for all remaining Prelude types and
    replace them by something more decent if there is something more decent.
-}

type a ->> b = Trans (DefaultChange a) (DefaultChange b)

-- * Channels in the ST monad

data Cell s a = Cell a (CellRef s a)

type CellRef s a = STRef s (Cell s a)

type Channel s a = STRef s (CellRef s a)

newChannel :: ST s (Channel s a, [a])
newChannel = do
    cellRef <- newSTRef undefined
    chan <- newSTRef cellRef
    let getContents cellRef = unsafeInterleaveST $ do
            Cell val cellRef' <- readSTRef cellRef
            vals <- getContents cellRef'
            return (val : vals)
            -- FIXME: Is this use of unsafeInterleaveST safe?
    contents <- getContents cellRef
    return (chan, contents)

writeChannel :: Channel s a -> a -> ST s ()
writeChannel chan val = do
    cellRef <- readSTRef chan
    cellRef' <- newSTRef undefined
    writeSTRef cellRef (Cell val cellRef')
    writeSTRef chan cellRef'

{-FIXME:
    Is there already an implementation of ST channels?
-}

{-FIXME:
    Remove Control.Monad.ST.Lazy.Unsafe from the import list, if the channel
    code moves to its own module.
-}

{-FIXME:
    The following things are to be considered:

      • Does our framework correspond to update lenses? How is it related to
        update lenses? Look at the slides of Tarmo’s seminar talk from
        11 September 2014.

      • Our work on order maintenance could be turned into a paper. Currently,
        one has to read more than one paper to understand the algorithm (Dietz
        and Sleator 1987; Willard 1986) and Dietz and Sleator (1987) do not
        explain deletion.

      • The incrementalized version of maps cannot allow conversion to sequences
        of key–value pairs, but only to sequences of values, because if the map
        was created from a sequence and was then converted to a sequence of
        key–value pairs, the choice of keys from equivalence classes of keys
        would depend on the history of changes to the original sequence, not
        just on the current value of the sequence.
-}
