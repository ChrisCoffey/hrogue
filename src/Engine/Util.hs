module Engine.Util (
    MapM(..),
    get,
    random',
    randomR',
    chooseN,
    nelChooseN,
    shuffle
) where


import Control.Monad (foldM)
import Control.Monad.State 
import Data.Monoid ((<>))
import Numeric.Natural
import System.Random

import qualified Data.List.NonEmpty as NEL

type MapM a = State StdGen a

random' :: (Random a) => MapM a
random' = state random

randomR' :: (Random a, Num a) => (a, a) -> MapM a
randomR' xs = state $ randomR xs

chooseN :: Natural -> [a] -> MapM [a]
chooseN 0 _ = pure []
chooseN n ls = fmap NEL.toList . nelChooseN n $ NEL.fromList ls

nelChooseN :: Natural -> NEL.NonEmpty a -> MapM (NEL.NonEmpty a)
nelChooseN count nel = do
    ix <- randomR' (0, NEL.length nel - 1)
    let (a, b:rest) = NEL.splitAt ix nel
    (b NEL.:|) <$> chooseN (count-1) (a<>rest)

shuffle :: [a] -> MapM [a]
shuffle ls = chooseN (fromIntegral $ length ls) ls
