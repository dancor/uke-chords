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
import Data.List
import Data.Maybe
import qualified Data.Map as Map

import ChordQual
import ChordSig
import Pitch
import PitchClass

majChord, minChord, dimChord, augChord :: [PitchClass]
majChord = [C, E, G]
minChord = [C, eFlat, G]
dimChord = [C, eFlat, gFlat]
augChord = [C, E, Gs]

domSevChord, majSevChord, minSevChord, dimSevChord :: [PitchClass]
domSevChord = [C, E, G, bFlat]
majSevChord = [C, E, G, B]
minSevChord = [C, eFlat, G, bFlat]
dimSevChord = [C, eFlat, gFlat, A]

halfDimSevChord, minMajSevChord, augMajSevChord, augDomSevChord ::
  [PitchClass]
halfDimSevChord = [C, eFlat, gFlat, bFlat]
minMajSevChord = [C, eFlat, G, B]
augMajSevChord = [C, E, Gs, B]
augDomSevChord = [C, E, Gs, bFlat]

domNinChord, majNinChord, minNinChord, domSevSharpNinChord ::
  [PitchClass]
domNinChord = [C, E, bFlat, D]
majNinChord = [C, E, B, D]
minNinChord = [C, eFlat, bFlat, D]
domSevSharpNinChord = [C, E, bFlat, Ds]  -- hendrix

chordSigs :: [ChordSig]
chordSigs =
  map (\ ((sig, rootIntvl), cq) -> ChordSig sig rootIntvl cq) $
  map (first pitchClassesToInfo)
  [
  (majChord, CQM),
  (minChord, CQm),
  (dimChord, CQdim),
  (augChord, CQaug),
  (domSevChord, CQ7),
  (majSevChord, CQM7),
  (minSevChord, CQm7),
  (dimSevChord, CQdim7),
  (halfDimSevChord, CQm7b5),
  (minMajSevChord, CQmM7),
  (augMajSevChord, CQaugM7),
  (augDomSevChord, CQaug7),
  (domNinChord, CQ9),
  (majNinChord, CQM9),
  (minNinChord, CQm9),
  (domSevSharpNinChord, CQ7S9)
  ]

maxFret :: Int
maxFret = 3

geetStrs :: [Pitch]
geetStrs = [(G, 4), (C, 4), (E, 4), (A, 4)]
--geetStrs = [(E, 1), (A, 1), (D, 2), (G, 2), (B, 2), (E, 3)]

functorRunSnd :: (Functor f) => (a, f b) -> f (a, b)
functorRunSnd = uncurry (fmap . (,))

pitchClassesToChordSig :: [PitchClass] -> Maybe ChordSig
pitchClassesToChordSig c =
  Map.lookup (fst $ pitchClassesToInfo c)
  (Map.fromList . map (\ x -> (csSig x, x)) $ chordSigs)

showFrets :: [Int] -> String
showFrets = concatMap show

myShowLine :: ChordSig -> [Int] -> (PitchClass, String)
myShowLine setChordSig frets =
  (pitchClass, showStr)
  where
  notesUniq = map head . group . sort $
    fretsToPitchClasses frets
  pitchClass = notesUniq !! (rootOffset `mod` length notesUniq)
  showStr =
    showFrets frets ++ ": " ++
    showPitchClass pitchClass ++
    cqShow (csQual setChordSig)
  rootOffset = csIntvlOrd setChordSig - fretRootOffset
  fretInfo = fretsToInfo frets
  (_, fretRootOffset) = fretInfo

myShowSet :: (ChordSig, [[Int]]) -> [String]
myShowSet (chordSig, fretss) =
  "" : title : map snd set
  where
  set = sort $ map (myShowLine chordSig) fretss
  title = cqShowLong (csQual chordSig) ++ " (" ++ counts ++ ")"
  counts = show cUniq ++
    (if cUniq == cTot then "" else "+" ++ show (cTot - cUniq))
  cUniq = length . nub $ map fst set
  cTot = length set

fretsToPitchClasses :: [Int] -> [PitchClass]
fretsToPitchClasses = map fst . zipWith pitchPlus geetStrs

fretsToChordSig :: [Int] -> Maybe ChordSig
fretsToChordSig = pitchClassesToChordSig . fretsToPitchClasses

fretsToInfo :: [Int] -> ([Int], Int)
fretsToInfo = pitchClassesToInfo . fretsToPitchClasses

main :: IO ()
main = do
  let
    goodFretss :: [[Int]]
    goodFretss =
      --filter ((<= 4) . length . filter (/= 0)) .
      filter (any (== 0)) .
      sequence $ replicate (length geetStrs) [0..maxFret]
    goodFretChordSigs :: [([Int], ChordSig)]
    goodFretChordSigs = catMaybes . map functorRunSnd . zip goodFretss $
      map fretsToChordSig goodFretss
    goodsByQual =
      Map.fromListWith (++) $
      map (\ (frets, chordSig) -> (csQual chordSig, [frets])) goodFretChordSigs
    thingsToShow = catMaybes $
      map (\ k -> functorRunSnd (k, Map.lookup (csQual k) goodsByQual))
      chordSigs
  putStr . unlines . concat $ map myShowSet thingsToShow
