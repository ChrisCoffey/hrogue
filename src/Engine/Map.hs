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
xMax = 100 
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

newLevel :: MapM Level
newLevel = pure . matrix yMax xMax $ const EmptySpace

makeMap :: StdGen -> [Level]
makeMap g = fst $ m g
    where m = runMap $ foldM (\acc _ -> (:acc) <$> makeLevel) [] [1..20]

makeLevel :: MapM Level
makeLevel = do
    el <- newLevel
    roomCount <- randomR' (3,9) :: MapM Int
    rooms <- traverse (const makeRoom) [1..roomCount]
    let rl = foldl drawRoom el rooms
    paths rl

drawRoom :: Level -> Room -> Level 
drawRoom lvl r@(ArbRoom l h _) = let
    verticalWalls = [(row, col)| row <- [fst l..fst h], col <- [snd l, snd h]]
    horizontalWalls = [(row, col)| row <- [fst l, fst h], col <- [snd l .. snd h]]
    l' = foldr (drawWall VWall) lvl verticalWalls
    l'' = foldr (drawWall HWall) l' horizontalWalls
    in fillRoom l'' r
    where
    drawWall w (row, col) lvl = 
        case safeGet row col lvl of
            Just EmptySpace -> fromMaybe lvl $ safeSet w (row, col) lvl
            _ -> lvl

fillRoom :: Level -> Room -> Level
fillRoom lvl (ArbRoom l h _) = let
    floor = [(row, col)| row <- [fst l + 1 .. fst h -1], col <- [snd l + 1 .. snd h -1]]
    in foldr (setElem Floor) lvl floor

makeRoom :: MapM Room
makeRoom = do
    lx <- randomR' (xMin, xMax - roomMaxWidth) 
    ly <- randomR' (yMin, yMax - roomMaxHeight)
    ux <- randomR' (lx + roomMinWidth, lx + roomMaxWidth)
    uy <- randomR' (ly + roomMinHeight, ly + roomMaxHeight)
    pure $ ArbRoom (ly, lx) (uy, ux) []

paths :: Level -> MapM Level
paths l = do
    pathCount <- randomR' (10, 10) :: MapM Int
    foldM (\x _ -> makePath x) l [1..pathCount]

makePath :: Level -> MapM Level
makePath l = do
    isVert <- random'
    col <- randomR' (xMin + 1, xMax -1)
    row <- randomR' (yMin + 1, yMax -1)
    if isVert
    then pure $ drawPath [((a, col-1), (a, col), (a, col+1)) | a <- [yMin + 1.. yMax -1]] True
    else pure $ drawPath [((row-1, a),(row, a), (row+1, a)) | a <- [xMin + 1.. xMax -1]] False
    where
    drawPath p True =  foldr (drawCell VWall) l p
    drawPath p False = foldr (drawCell HWall) l p
    drawCell w (l,i,r) lvl = 
        case lvl ! i of
            Floor -> lvl
            EmptySpace -> let
                lvl' = fromMaybe lvl $ safeSet w l lvl
                lvl'' = fromMaybe lvl $ safeSet Floor i lvl'
                in fromMaybe lvl $ safeSet w r lvl''
            HWall -> fromMaybe lvl $ safeSet Door i lvl
            VWall -> fromMaybe lvl $ safeSet Door i lvl
            _ -> lvl


