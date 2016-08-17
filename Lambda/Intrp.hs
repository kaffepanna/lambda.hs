module Lambda.Intrp(intrp) where

import Lambda.Stmt
import Control.Monad
import Data.Either
import Debug.Trace

newtype Intrp a = Intrp { runItrp :: Scope -> Either IntrpError (a, Scope) }
data IntrpError = IntrpError String deriving (Show)

instance Functor Intrp where
    fmap = liftM

instance Applicative Intrp where
    pure = return
    (<*>) = ap

instance Monad Intrp where
    return x = Intrp $ \r -> Right (x, r)
    i >>= k = Intrp $ \r -> case runItrp i r of
            Left msg -> Left msg
            Right (x, r') -> runItrp (k x) r'
    fail msg = Intrp $ \_ -> Left $ IntrpError msg

push :: String -> Stmt -> Intrp()
push v x = Intrp $ \r -> Right ((), (v,x):r)

get :: String -> Intrp (Stmt)
get v = Intrp $ \r -> case lookup v r of
    Nothing -> Right (V v, r)
    Just v  -> Right (v, r)

pop :: Intrp ()
pop = Intrp $ \r -> Right ((), tail r)

reduce (V n) = do
    v <- get n
    traceM $ "get var: " ++ show (V n) ++ " --> " ++ show v
    return v

reduce (L v s) = do
    traceM $ "reduce: " ++ show (L v s)
    s' <- reduce s
    return $ L v s'

reduce (A (L v s) r) = do
    traceM $ "apply: " ++ show r ++ " on " ++ show (L v s)
    r' <- reduce r
    push v r'
    s' <- reduce s
    pop
    return s'

reduce (A l r) = do
    traceM $ "apply: " ++ show r ++ " on " ++ show l
    l' <- reduce l
    r' <- reduce r
    if l' == l then return $ A l' r'
    else reduce (A l' r')

intrp p = case lookup "main" p of
    Just main -> fmap fst (runItrp (reduce main) p)
    Nothing -> Left $ IntrpError "main not defined"

