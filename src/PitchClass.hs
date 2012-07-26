module PitchClass where

import Control.Applicative()
import Control.Monad
import Data.List
import Data.Ord

data PitchClass = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B
  deriving (Show, Eq, Ord, Read, Enum, Bounded)

aFlat, bFlat, dFlat, eFlat, gFlat :: PitchClass
aFlat = Gs
bFlat = As
dFlat = Cs
eFlat = Ds
gFlat = Fs

showPitchClass :: PitchClass -> String
showPitchClass Cs = "C#"
showPitchClass Ds = "Eb"
showPitchClass Fs = "F#"
showPitchClass Gs = "Ab"
showPitchClass As = "Bb"
showPitchClass x = show x

octaveInt :: Int
octaveInt = 1 + fromEnum (maxBound :: PitchClass)

pitchClassesToInfo :: [PitchClass] -> ([Int], Int)
pitchClassesToInfo =
  markedIntervalsToChordSig .
  onAdjPairs (\ (a, aMark) (b, _) -> (b - a, aMark)) .
  octowrap .
  markFirst .
  map fromEnum .
  map head . group . sort
  where
  --octowrap l = l ++ [head l + octaveInt]
  octowrap l = l ++ [(fst (head l) + octaveInt, snd $ head l)]

markedIntervalsToChordSig :: [(Int, Bool)] -> ([Int], Int)
markedIntervalsToChordSig intvlsMarked =
  (\ x -> (map fst x, indexOf (== True) (map snd x))) $
  bestSig intvlsMarked
  where
  bestSig :: [(Int, a)] -> [(Int, a)]
  bestSig l = maximumBy (comparing (map fst)) .
    take n $ allCyclesForever l where n = length l

adjPairs :: [a] -> [(a, a)]
adjPairs = ap zip tail

onAdjPairs :: (b -> b -> c) -> [b] -> [c]
onAdjPairs f = map (uncurry f) . adjPairs

allCyclesForever :: [t] -> [[t]]
allCyclesForever l = l : allCyclesForever (rest ++ [l0]) where (l0:rest) = l

indexOf :: (Enum c, Num c) => (b -> Bool) -> [b] -> c
indexOf f = fst . head . filter (f . snd) . zip [0..]

markFirst :: [a] -> [(a, Bool)]
markFirst x =  zip x (True : repeat False)
