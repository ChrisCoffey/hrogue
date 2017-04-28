module Engine.Level.Room 
where

import Prelude hiding (head, filter)
import Engine.Util
import Data.List.NonEmpty hiding (length, elem)

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
    deriving (Eq, Ord, Show)

randomRoom :: NonEmpty Rect -> MapM (NonEmpty Rect, Room)
randomRoom rects = do
    rect <- randomRect rects
    let lx = x $ left rect
        ly = y $ left rect
        hx = x $ right rect
        hy = y $ right rect
    dx <- (+2) <$> randomR' (0, if hx - lx > 28 then 12 else 8)
    dy' <- (+2) <$> randomR' (0, 4)
    let dy = if dx * dy' > 50 then 50 `div` dx else dy'
    let xBorder = if lx > 0 && hx < colNum -1 then 2 * xLim else xLim + 1
    let yBorder = if ly > 0 && hy < rowNum -1 then 2 * yLim else yLim + 1
    xabs <- (+ (lx + if lx > 0 then xLim else 3)) <$> randomR' (0, hx - (if lx > 0 then lx else 3) - dx - xBorder + 1)
    yabs' <- (+ (ly + if ly > 0 then yLim else 2)) <$> randomR' (0, hy - (if ly > 0 then ly else 2) - dy - yBorder + 1)
    yabs <- if ly == 0 && hy >= rowNum -1 && yabs' + dy > rowNum `div` 2 -- Chose to drop the (!nroom || !rn2(nroom)) condition b/c I don't know what it does
            then randomR' (2,5)
            else pure yabs'
    -- Skipped the dy adjustment based on 'nroom'
    let wtmp = dx + 1
        htmp = dy + 1
        r2 = Rect {
        left = Point (xabs -1) (yabs -1),
        right = Point (xabs + wtmp) (yabs + htmp)
        }
    -- Add the validations that rerun this on failure
    pure (splitRects rect r2 rects, Ordinary r2) -- Just a temp hack

--
-- Rectangle Managment
--

-- Pull an arbitrary rectangle from the list
randomRect :: NonEmpty Rect  -> MapM Rect
randomRect = fmap head . nelChooseN 1

initialRect :: NonEmpty Rect
initialRect = Rect (Point 0 0) (Point (rowNum -1) (colNum -1)) :| []

removeRect :: Rect -> NonEmpty Rect -> [Rect]
removeRect r = filter (/= r) 

addRect :: Rect -> [Rect] -> [Rect]
addRect r rs 
    | length rs >= maxRect = rs --Maybe don't silently drop the rect? But it doesn't really matter
    | elem r rs = rs
    | otherwise = r:rs

splitRects :: Rect -> Rect -> NonEmpty Rect -> NonEmpty Rect
splitRects a b rs = 
    fromList $ go a b (toList rs)
    where
    go r1 r2 rects = let
        rs' = removeRect r1 rs
        in check4 . check3 . check2 . check1 $ resolveCollisions rs'
        where
        resolveCollisions rects =  foldr f rects rects
            where 
                f rect acc
                    | intersect rect r2 = go rect (overlap rect r2) acc
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
