module Render.Ascii where

import Engine.Types

import Control.Monad (forM_)
import Data.Matrix

render :: Level -> IO ()
render = mapM_ printLine . toLists
    where
    printLine = print . foldr f "" 
    f HWall s = '-':s
    f VWall s = '|':s
    f Door s = '/':s
    f Floor s = '.':s
    f EmptySpace s = ' ':s
