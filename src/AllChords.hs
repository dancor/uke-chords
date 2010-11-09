-- playing around with:
-- - generating all possible chords
-- - generating (tertian) names for them
-- - generating fingerings for them

-- cc is pitchclass count (usually 12)

import Control.Arrow
import Data.Function
import Data.List
import qualified Data.Map as M

type Chord = [Int]

normalizeChord cc c = init . minimum $ zipWith (++) (tails c') (inits c')
  where
  c' = cc - sum c : c

nChords :: Int -> Int -> [Chord]
nChords cc n = if n > cc then [] else nub . map (normalizeChord cc) $ concat 
  [map (i1:) (cOSNaiveOver cc (cc - i1) (n - 1) i1) | i1 <- [1..div cc n]]

cOSNaiveOver _ _ 1 _ = [[]]
cOSNaiveOver cc ccLeft n i = concat
  [map (i1:) (cOSNaiveOver cc (ccLeft - i1) (n - 1) i) | i1 <- [i..ccLeft - i]]

diffs :: [Int] -> [Int]
diffs (a1:a2:as) = (a2 - a1) : diffs (a2:as)
diffs _ = []

makeChord :: Int -> [(String, Int)] -> (Chord, [String])
makeChord n l = (
  normalizeChord 12 . map (`mod` 12) . diffs $ 0 : sort (map snd l),
  map fst l)

i3 = [("b3", 3), ("M3", 4), ("sus4", 5)]
i5 = [("b5", 6), ("M5", 7), ("#5", 8)]
i7 = [("bb7", 9), ("b7", 10), ("M7", 11)]
i9 = [("b9", 1), ("M9", 2)]
--i9 = [("b9", 1), ("M9", 2), ("#9", 3)]
i6 = [("#6", 10)]

choose :: Int -> [a] -> [[a]]
choose 0 [] = [[]]
choose _ [] = []
choose n (a:l) = map (a:) (choose (n - 1) l) ++ choose n l

interestingChords :: Int -> Int -> [(Chord, [String])]
interestingChords cc n = filter ((/= 0) . head . fst) . map (makeChord n) .
  concatMap sequence $ choose (n - 1) [i3, i5, i6, i7, i9]

ukeOffsets = [7, 0, 4, 9]

offsetsToChord cc = normalizeChord cc . diffs . sort

main = do
  let
    cc = 12
    n = 4
    chordNameMap = M.fromListWith (++) $
      [(c, []) | c <- nChords cc n] ++ 
      map (second (:[])) (interestingChords cc n)
    remCr1 s l = if length l <= 1 then l else filter (not . s) l
    removeCrapChords = remCr1 (elem "sus4") . remCr1 (elem "#6")
    chordPlayMap = M.fromListWith (++) . filter ((/= 0) . head . fst) .
      map (\ o -> (offsetsToChord cc $ zipWith (+) ukeOffsets o, [o])) . 
      sequence $ replicate (length ukeOffsets) [0..11]
    showPlay = concatMap show
    showChords = show . map concat . removeCrapChords
  putStrLn . unlines . 
    map (\ (k, (v1, v2)) -> show k ++ " " ++ showPlay 
      (minimumBy (compare `on` maximum) v1) ++ "  " ++ showChords v2) .
    M.toList $ M.intersectionWith (,) chordPlayMap chordNameMap
