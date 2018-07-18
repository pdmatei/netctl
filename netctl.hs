{-# LANGUAGE ForeignFunctionInterface, CPP #-}

-- -XScopedTypeVariables

import qualified Data.Map.Strict as Map
import Control.Monad


data Formula = Atomic SEFL | EF Formula | AF Formula

data SEFL = Atom String | IB [SEFL] | Fork [SEFL]
type State = (SEFL,String) -- the state keeps the current code as well as some store, denoted here as string



-- --------------------------------
-- symbolic execution
-- --------------------------------

data MS a = MS (State -> [State])

instance Monad MS where
{-    
    (MS m) >>= f = MS (\s -> case (m s) of
                            [] -> []             -- x is unimportant, gluing is
                            (x:xs) -> let (MS m') = f x in foldr (++) [] (map m' (x:xs)))
-}
    (MS m) >> (MS m') = MS (\s -> case m s of
                                [] -> []
                                l -> foldr (++) [] (map m' l))
    
    return x = MS (\s->[s])


sem :: SEFL -> MS ()
sem (Atom "Assign") = MS (\s->[])
-- etc for all atomic instructions


sem (IB (x:xs)) = do {
                    (sem x);
                    (sem (IB xs))
                }


next :: State -> [State]
next (p,s) = let (MS m) = sem p in m s 


-- --------------------------------
-- monadic type & operations
-- --------------------------------
data M a = M (State -> a)

-- data M a = M (State -> (a,[State]))


instance Monad M where
    (M m) >>= f = M (\s-> let (M m') = (f (m s)) in m' s)
    return x = M (\s->x)

-- step takes a monad, a binary folding operator, an initial value, and returns a 'folding' monad
-- when called on state s, the monad performs m on the successors of s, by following the logic of
-- the operator; as long as (next s) is constructed lazily, folding may explore only some successors
-- of the state at hand

step :: M a -> (a -> b -> b) -> b -> M b
step (M m) op init = M (\s->foldr op init [m s' | s' <- next s])


-- takes a boolean monad, and applies it the successors of the supplied state
-- and performs folding with AND
someTrue :: M Bool -> M Bool
someTrue m = step m (||) False

allTrue :: M Bool -> M Bool
allTrue m = step m (&&) True

-- STUB
check :: SEFL -> M Bool
check p = return True    -- here we need to perform symbolic execution ...


-- --------------------------------
-- interpreter
-- --------------------------------

-- trivial approach
eval :: Formula -> M Bool

-- (eval f) un verificator pt f si orice cod SEFL

eval (Atomic p) = check p
eval (EF f) = do {
                    b <- (eval f) ;
                    if b then return True else someTrue (eval (EF f))
                    }

eval (AF f) = do {
                    b <- (eval f);
                    if b then return True else allTrue (eval (AF f))
              }






