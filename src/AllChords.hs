-- This:
-- - generates all possible chords
-- - generates (tertian) names for them
-- - generates fingerings for them

import Control.Arrow
import Data.Function
import Data.List
import qualified Data.Map as M

-- | A "chord" is a series of intervals that add up to < octaveSize
-- (strictly less!).
type Chord = [Int]

allCycles :: [a] -> [[a]]
allCycles xs = zipWith (++) (tails xs) (inits xs)

-- | Get a representative chord under the equivalence relation that
-- combines any two chords that differ only under changing-the-bass-note.
normalizeChord :: Int -> Chord -> Chord
normalizeChord octaveSize c = init . minimum $ allCycles cFillOctave
  where
    cFillOctave = octaveSize - sum c : c

allNormChords :: Int -> Int -> [Chord]
allNormChords octaveSize noteCount = if noteCount > octaveSize
    then []
    else nub . map (normalizeChord octaveSize) $ concat
         [ map (firstIntvl:) $ chordsNaive octaveSize
           (octaveSize - firstIntvl) (noteCount - 1) firstIntvl
         | firstIntvl <- [1 .. octaveSize `div` noteCount]
         ]

chordsNaive :: Int -> Int -> Int -> Int -> [Chord]
chordsNaive _ _ 1 _ = [[]]
chordsNaive octaveSize octaveSizeLeft noteCount intvlBound = concat
    [ map (intvl1:) (chordsNaive octaveSize (octaveSizeLeft - intvl1)
      (noteCount - 1) intvlBound)
    | intvl1 <- [intvlBound .. octaveSizeLeft - intvlBound]
    ]

diffs :: [Int] -> [Int]
diffs (a1:a2:as) = (a2 - a1) : diffs (a2:as)
diffs _ = []

makeChord :: Int -> [(String, Int)] -> (Chord, [String])
makeChord octaveSize l =
    ( normalizeChord octaveSize . map (`mod` octaveSize) . diffs $
      0 : sort (map snd l)
    , map fst l
    )

-- | Uses M for Major tertian tonic notes, +/- for sharp/flat, and s for
-- off-tonic "suspensions".
i3, i5, i7, i9, i6 :: [(String, Int)]
i3 = [("-3", 3), ("M3", 4), ("s4", 5)]
i5 = [("-5", 6), ("M5", 7), ("+5", 8)]
i7 = [("-7", 10), ("M7", 11)]
i9 = [("-9", 1), ("M9", 2)]
i6 = [("s6", 9), ("+6", 10)]

choose :: Int -> [a] -> [[a]]
choose 0 [] = [[]]
choose _ [] = []
choose n (a:l) = map (a:) (choose (n - 1) l) ++ choose n l

interestingChords :: Int -> Int -> [(Chord, [String])]
interestingChords octaveSize noteCount = filter ((/= 0) . head . fst) .
    map (makeChord octaveSize) . concatMap sequence $
    choose (noteCount - 1) [i3, i5, i7, i9, i6]

ukeOffsets :: [Int]
ukeOffsets = [7, 0, 4, 9]

offsetsToChord :: Int -> [Int] -> Chord
offsetsToChord octaveSize = normalizeChord octaveSize . diffs . sort

main :: IO ()
main = do
    let octaveSize = 12
        noteCount = 4
        chordNameMap = M.fromListWith (++) $
            [(c, []) | c <- allNormChords octaveSize noteCount] ++
            map (second (:[])) (interestingChords octaveSize noteCount)
        remCr1 s l = if length l <= 1 then l else filter (not . s) l
        removeCrapChords = remCr1 ("+5" `elem`) .
                           remCr1 ("s6" `elem`) . remCr1 ("+6" `elem`)
        chordPlayMap = M.fromListWith (++) . filter ((/= 0) . head . fst) .
            map (\ o -> ( offsetsToChord octaveSize $ zipWith (+) ukeOffsets o
                        , [o]
                        )
                ) .
            sequence $ replicate (length ukeOffsets) [0 .. octaveSize - 1]
        showPlay = concatMap show
        showChords = show . map concat . removeCrapChords
    putStrLn . unlines .
        map (\ (k, (v1, v2)) -> show k ++ " " ++ showPlay
            (minimumBy (compare `on` maximum) v1) ++ "  " ++ showChords v2) .
        M.toList $ M.intersectionWith (,) chordPlayMap chordNameMap
