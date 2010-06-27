-- We want to find all chords of common types that use an open string
-- and don't go too high on the fretboard.

-- Music terms:
-- - note
-- - pitch
-- - pitch class
-- - timbre
-- - tonality
-- - key: we decide that key is always major key..
-- - scale: ..and scale can be major or minor or other
-- - scale degree
-- - triad: three-note chord of two thirds
-- - tone: note or pitch or timbre or tonality
-- - root: reference note of a chord
-- - tonic: first scale degree
--   tonic
--   supertonic
--   mediant
--   subdominant
--   dominant
--   submediant
--   leading tone
-- Our nonce terms:
-- - A "chord sig" is the pairwise half-step-distances formed by pitch classes,
--   in the inversion of the chord that maximizes the first distance (then the
--   second on a tie, and so on).

import Control.Arrow
import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import qualified Data.Map as Map

data PitchClass = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B
  deriving (Show, Eq, Ord, Read, Enum, Bounded)

aFlat = Gs
bFlat = As
dFlat = Cs
eFlat = Ds
gFlat = Fs

type Pitch = (PitchClass, Int)

octaveInt :: Int
octaveInt = 1 + fromEnum (maxBound :: PitchClass)

pitchToInt :: Pitch -> Int
pitchToInt = uncurry (+) . second (octaveInt *) . first fromEnum

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

intToPitch :: Int -> Pitch
intToPitch = first toEnum . swap . (`divMod` octaveInt)

pitchDiff :: Pitch -> Pitch -> Int
pitchDiff = (-) `on` pitchToInt

pitchPlus :: Pitch -> Int -> Pitch
pitchPlus n = intToPitch . (pitchToInt n +)

adjPairs :: [a] -> [(a, a)]
adjPairs = ap zip tail

onAdjPairs :: (b -> b -> c) -> [b] -> [c]
onAdjPairs f = map (uncurry f) . adjPairs 

allCyclesForever :: [t] -> [[t]]
allCyclesForever l = l : allCyclesForever (rest ++ [l0]) where (l0:rest) = l

pitchClassesToChordSig :: [PitchClass] -> [Int]
pitchClassesToChordSig = bestSig . 
  onAdjPairs (flip (-)) . octowrap . map fromEnum . map head . group . sort
  where
  octowrap :: [Int] -> [Int]
  octowrap l = l ++ [head l + octaveInt]
  bestSig :: [Int] -> [Int]
  bestSig l = maximum . take n $ allCyclesForever l where n = length l

majChord = [C, E, G]
minChord = [C, eFlat, G]
dimChord = [C, eFlat, gFlat]
augChord = [C, E, Gs]

domSevChord = [C, E, G, bFlat]
majSevChord = [C, E, G, B]
minSevChord = [C, eFlat, G, bFlat]
dimSevChord = [C, eFlat, gFlat, A]

halfDimSevChord = [C, eFlat, gFlat, bFlat]
minMajSevChord = [C, eFlat, G, B]
augMajSevChord = [C, E, Gs, B]
augDomSevChord = [C, E, Gs, bFlat]

domNinChord = [C, E, bFlat, D]
majNinChord = [C, E, B, D]
minNinChord = [C, eFlat, bFlat, D]
domSevAugNinChord = [C, E, bFlat, Ds]  -- hendrix

chordSigs :: [([Int], String)]
chordSigs = map (first pitchClassesToChordSig) [
  (majChord, "M"),
  (minChord, "m"),
  (dimChord, "dim"),
  (augChord, "aug"),
  (domSevChord, "7"),
  (majSevChord, "M7"),
  (minSevChord, "m7"),
  (dimSevChord, "dim7"),
  (halfDimSevChord, "m7b5"),
  (minMajSevChord, "mM7"),
  (augMajSevChord, "augM7"),
  (augDomSevChord, "aug7"),
  (domNinChord, "9"),
  (majNinChord, "M9"),
  (minNinChord, "m9"),
  (domSevAugNinChord, "7#9")
  ]

maxFret :: Int
maxFret = 3

geetStrs :: [Pitch]
geetStrs = [(G, 3), (C, 3), (E, 3), (A, 3)]

functorRunSnd :: (Functor f) => (a, f b) -> f (a, b)
functorRunSnd = uncurry (fmap . (,))

chordGetName :: [PitchClass] -> Maybe String
chordGetName = flip Map.lookup (Map.fromList chordSigs) . 
  pitchClassesToChordSig

main :: IO ()
main = do
  let
    frets = 
      filter (any (== 0)) . sequence $ replicate (length geetStrs) [0..maxFret]
    fretsToName = catMaybes . map functorRunSnd .  zip frets $ 
      map (chordGetName . map fst . zipWith pitchPlus geetStrs) frets
    nameToFrets = Map.fromListWith (++) $ map (swap . first (:[])) fretsToName
    myShow (name, fretss) = map ((++ name) . (++ ": ") . showFrets) fretss
    showFrets l = concatMap show l
  putStr . unlines . concat . map (myShow . second sort) . catMaybes $ 
    map ((\ k -> functorRunSnd (k, Map.lookup k nameToFrets)) . snd) chordSigs
