module Engine.Map (
    MapM (..),
    makeMap,
    makeLevel
) where

import qualified Data.Vector as V

import Engine.Types

import Control.Monad (foldM)
import Data.Monoid ((<>))
import Data.Matrix hiding (trace)
import Data.Maybe (fromMaybe, maybe)
import Numeric.Natural
import System.Random

import Debug.Trace

-- Grid is 100x160
xMin, xMax, roomMinWidth, roomMaxWidth, yMin, yMax, roomMinHeight, roomMaxHeight :: Int
xMin = 1
xMax = 160
roomMinWidth = 6
roomMaxWidth = 50
yMin = 1 
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

random' :: (Random a) => MapM a
random' = MapM $ \s -> random s

randomR' :: (Random a, Num a) => (a, a) -> MapM a
randomR' xs = MapM $ \s -> randomR xs s

chooseN :: Natural -> [a] -> MapM [a]
chooseN 0 _ = pure []
chooseN n ls = do
    ix <- randomR' (0, length ls - 1)
    let (a, b:rest) = splitAt ix ls
    (b:) <$> chooseN (n-1) (a<>rest)

shuffle :: [a] -> MapM [a]
shuffle ls = chooseN (fromIntegral $ length ls) ls

data Room = ArbRoom {ll:: Cell, ur:: Cell, doors:: [Cell]} 
    deriving Show

data Path = Path {trail :: [Cell]}

newLevel :: MapM Level
newLevel = pure . matrix yMax xMax $ const Floor

makeMap :: StdGen -> [Level]
makeMap g = fst $ m g
    where m = runMap $ foldM (\acc _ -> (:acc) <$> makeLevel) [] [1..20]

makeLevel :: MapM Level
makeLevel = do
    el <- newLevel
    pl <- paths el
    roomCount <- randomR' (6,17) :: MapM Int
    rooms <- traverse (const makeRoom) [1..roomCount]
    let (passingRooms, lvl) = foldl (\ (rs,l) r -> if checkRoom r l then (r:rs, reifyRoom r l) else tryShiftRoom r (rs,l)) 
                                    ([], pl) 
                                    rooms
    pure lvl
    where
    checkRoom (ArbRoom (x,y) (x',y') _) l = let
        a = all (== Floor) . V.take (x' - x) . V.drop x $ getRow y l
        b = all (== Floor) . V.take (x' - x) . V.drop x $ getRow y' l
        c = all (== Floor) . V.take (y' - y) . V.drop y $ getCol x l
        d = all (== Floor) . V.take (y' - y) . V.drop y $ getCol x' l
        in inBounds x xMin xMax && 
           inBounds x' xMin xMax &&
           inBounds y yMin yMax &&
           inBounds y' yMin yMax &&
             a && b && c && d
    tryShiftRoom r (xs, l) = case filter (`checkRoom` l) rs of
        [] -> (xs, l)
        (x:_) -> (x:xs, reifyRoom x l)
        where
        rs = roomShifts r
    reifyRoom (ArbRoom (x,y) (x',y') _) l = let
        l' = foldl (\ac i -> fromMaybe ac $ safeSet HWall i ac) l [(b,a)| a <- [x..x'], b <- [y, y']]
        in foldl (\ac i -> fromMaybe ac $ safeSet VWall i ac) l' [(a, b)| a <- [y..y'], b <- [x, x']]

makeRoom :: MapM Room
makeRoom = do
    lx <- randomR' (xMin, xMax - roomMaxWidth) 
    ly <- randomR' (yMin, yMax - roomMaxHeight)
    ux <- randomR' (lx + roomMinWidth, lx + roomMaxWidth)
    uy <- randomR' (ly + roomMinHeight, ly + roomMaxHeight)
    pure $ ArbRoom (lx, ly) (ux, uy) []

roomShifts :: Room -> [Room]
roomShifts (ArbRoom (x,y) (x',y') _) = let
    width = x' - x
    height = y' - y 
    in [ArbRoom (a,b) (a+width, b + height) [] | a <- [x - 10.. x + 10], b <- [y - 10.. y + 10]]
      
paths :: Level -> MapM Level
paths l = do
    pathCount <- randomR' (10, 30) :: MapM Int
    foldM (\x _ -> makePath x) l [1..pathCount]

makePath :: Level -> MapM Level
makePath l = do
    isVert <- random'
    col <- randomR' (xMin + 1, xMax -1)
    row <- randomR' (yMin + 1, yMax -1)
    if isVert
    then pure $ drawPath [((a, col-1), (a, col), (a, col+1)) | a <- [yMin + 1.. yMax -1]] False True
    else pure $ drawPath [((row-1, a),(row, a), (row+1, a)) | a <- [xMin + 1.. xMax -1]] False False
    where
    drawPath p inside True = fst $ foldl (drawCell VWall) (l, False) p
    drawPath p inside False = fst $ foldl (drawCell HWall) (l, False) p
    drawCell w (lvl,inside) (l,i,r) = 
        case lvl ! i of
            Floor -> if inside 
                     then (lvl, inside) 
                     else let
                        lvl' = fromMaybe lvl $ safeSet w l lvl
                        in (fromMaybe lvl $ safeSet w r lvl', False)
            HWall -> (fromMaybe lvl $ safeSet Door i lvl, not inside)
            VWall -> (fromMaybe lvl $ safeSet Door i lvl, not inside)
            _ -> if inside 
                 then (lvl, False) 
                 else (lvl, inside)

inBounds :: Int -> Int -> Int -> Bool
inBounds x low high = x >= low && x <= high
