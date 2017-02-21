module Engine.Map (
    makeMap,
    makeLevel
) where

import qualified Data.Vector as V

import Engine.Types

import Control.Monad (foldM)
import Data.Matrix
import System.Random

-- Grid is 100x160
xMin, xMax, roomMinWidth, roomMaxWidth, yMin, yMax, roomMinHeight, roomMaxHeight :: Int
xMin = 0
xMax = 160
roomMinWidth = 6
roomMaxWidth = 50
yMin = 0
yMax = 100
roomMinHeight = 6
roomMaxHeight = 40

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

random' :: (Random a, Num a) => MapM a
random' = MapM $ \s -> random s

randomR' :: (Random a, Num a) => (a, a) -> MapM a
randomR' xs = MapM $ \s -> randomR xs s

data Room = ArbRoom {ll:: Cell, ur:: Cell, doors:: [Cell]} 

newLevel :: MapM Level
newLevel = pure . matrix xMax yMax $ const Floor

makeMap :: StdGen -> [Level]
makeMap g = fst $ m g
    where m = runMap $ foldM (\acc _ -> (:acc) <$> makeLevel) [] [1..20]

makeLevel :: MapM Level
makeLevel = do
    roomCount <- randomR' (6,17) :: MapM Int
    rooms <- traverse (const makeRoom) [0..roomCount]
    emptyLvl <- newLevel
    let lvl = foldl (\l r-> if checkRoom r l then reifyRoom r l else l) emptyLvl rooms
    pure lvl
    where
    checkRoom (ArbRoom (x,y) (x',y') _) l = let
        a = all (== Floor) . V.take (x' - x) . V.drop x $ getRow y l
        b = all (== Floor) . V.take (x' - x) . V.drop x $ getRow y' l
        c = all (== Floor) . V.take (y' - y) . V.drop y $ getCol x l
        d = all (== Floor) . V.take (y' - y) . V.drop y $ getCol x' l
        in a && b && c && d
    reifyRoom (ArbRoom (x,y) (x',y') _) l = let
        l' = foldl (flip (setElem HWall)) l [(a,b)| a <- [x..x'], b <- [y, y']]
        in foldl (flip (setElem VWall)) l' [(b, a)| a <- [y..y'], b <- [x, x']]

makeRoom :: MapM Room
makeRoom = do
    lx <- randomR' (xMin, xMax - roomMaxWidth) 
    ly <- randomR' (yMin, yMax - roomMaxHeight)
    ux <- randomR' (lx, lx + roomMaxWidth)
    uy <- randomR' (ly, ly + roomMaxHeight)
    pure $ ArbRoom (lx, ly) (ux, uy) []
