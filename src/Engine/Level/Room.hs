module Engine.Level.Room 
where

import Prelude hiding (head, filter, elem, length)
import Engine.Util
import Data.List.NonEmpty

--Max Dimensions
colNum = 100
rowNum = 40
maxRect = 50
xLim = 4
yLim = 3

data Point = Point {x :: Int, y :: Int }
    deriving (Eq, Ord, Show)

data Rect = Rect {
    left:: Point,
    right:: Point
    }
    deriving (Eq, Ord, Show)

data Room = Ordinary {rect :: Rect}

-- Pull an arbitrary rectangle from the list
randomRect :: NonEmpty Rect  -> MapM Rect
randomRect = fmap head . nelChooseN 1

initialRect :: NonEmpty Rect
initialRect = Rect (Point 0 0) (Point (rowNum -1) (colNum -1)) :| []

removeRect :: Rect -> NonEmpty Rect -> [Rect]
removeRect r = filter (/= r) 

addRect :: Rect -> NonEmpty Rect -> NonEmpty Rect
addRect r rs 
    | length rs >= maxRect = rs --Maybe don't silently drop the rect? But it doesn't really matter
    | elem r rs = rs
    | otherwise = r <| rs

splitRects :: Rect -> Rect -> NonEmpty Rect -> NonEmpty Rect
splitRects r1 r2 rs = let
    rs' = removeRect r1 rs
    in check4 . check3 . check2 . check1 $ resolveCollisions (fromList rs')
    where
    resolveCollisions rects =  foldr f rects rects
        where 
            f rect acc
                | intersect rect r2 = splitRects rect (overlap rect r2) acc
                | otherwise = acc
    check1 rects = if (y $ left r2) - (y . left $ r1) - 1 > (if (y . right $ r1) < rowNum -1 then 2 * yLim else yLim + 1) + 4
                   then addRect (r1 {right = (right r1) {y = (y . left $ r2) - 2}}) rects
                   else rects
    check2 rects = if (x $ left r2) - (x . left $ r1) - 1 > (if (x . right $ r1) < colNum -1 then 2 * xLim else xLim + 1) + 4
                   then addRect (r1 {right = (right r1) {x = (x . left $ r2) - 2}}) rects
                   else rects
    check3 rects = if (y $ right r1) - (y $ right r2) - 1 > (if (y . left $ r1) > 0 then 2 * yLim else yLim + 1) + 4
                   then addRect (r1 {left = (left r1) {y = (y . right $ r2) + 2}}) rects
                   else rects
    check4 rects = if (x $ right r1) - (x $ right r2) - 1 > (if (x . left $ r1) > 0 then 2 * xLim else xLim + 1) + 4
                   then addRect (r1 {left = (left r1) {x = (x . right $ r2) + 2}}) rects
                   else rects

intersect :: Rect -> Rect -> Bool
intersect l r = 
    simpleIntersect || let
        ol = overlap l r
        in not $ (x . left $ ol) > (x . right $ ol) || (y . left $ ol) > (y . right $ ol)
    where
    simpleIntersect = 
       (x . left $ r) > (x . right $ l) || 
       (y . left $ r) > (y . right $ l) ||
       (x . right $ r) < (x . left $ l) || 
       (y . right $ r) < (y . left $ l)
        
overlap :: Rect -> Rect -> Rect
overlap l r = let
    lx = if ((x . left $ r) > (x . left $ l)) then x . left $ l else x.left$r
    ly = if ((y . left $ r) > (y . left $ l)) then y.left$l else y . left $ r
    hx = if ((x . right $ r) > (x . right $ l)) then x.right$l else x.right$r
    hy = if ((y . right $ r) > (y . right $ l)) then y.right$l else y.right$r
    in Rect (Point lx ly) (Point hx hy)

-- General helpers
elem :: Eq a => a -> NonEmpty a -> Bool
elem a = not . null . filter (== a)
