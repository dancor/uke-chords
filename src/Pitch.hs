module Pitch where

import Control.Arrow
import Data.Function

import PitchClass

type Pitch = (PitchClass, Int)

intToPitch :: Int -> Pitch
intToPitch = first toEnum . swap . (`divMod` octaveInt)

pitchDiff :: Pitch -> Pitch -> Int
pitchDiff = (-) `on` pitchToInt

pitchPlus :: Pitch -> Int -> Pitch
pitchPlus p n = intToPitch (pitchToInt p + n)

pitchToInt :: Pitch -> Int
pitchToInt = uncurry (+) . second (octaveInt *) . first fromEnum

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)
