module Main where

import Render.Ascii
import Engine.Map

import System.Random

main :: IO ()
main = render . fst . runMap makeLevel =<< newStdGen
