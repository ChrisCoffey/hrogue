module Engine.Map (
    makeMap,
    makeLevel
) where

import Engine.Types

import Control.Monad (foldM)
import System.Random

-- Grid is 100x160

newtype MapM a = MapM {runMap :: StdGen -> (a, StdGen)}

instance Functor MapM where
    fmap f (MapM g) = MapM $ \s -> 
        let (a, s') = g s
        in (f a, s')

instance Applicative MapM where
    pure a = MapM $ \s -> (a, s)
    (MapM f) <*> (MapM act) = MapM $ \s -> let
        (a, s') = act s
        (g, s'') = f s'
        in (g a, s'')

instance Monad MapM where
    return a = MapM $ \s -> (a, s)
    MapM f >>= m = MapM $ \s ->
        let (a, s') = f s
        in runMap (m a) s'

get :: MapM StdGen 
get = MapM $ \s -> (s,s)

data Room = ArbRoom {lr:: Cell, ur:: Cell, doors:: [Cell]} 

makeMap :: StdGen -> [Level]
makeMap g = fst $ m g
    where m = runMap $ foldM (\acc _ -> (:acc) <$> makeLevel) [] [1..20]

makeLevel :: MapM Level
makeLevel = do
    (n, _) <- random <$> get :: MapM (Int, StdGen)
    let x = (n + 1)
    pure undefined

makeRoom :: MapM Room
makeRoom = undefined
