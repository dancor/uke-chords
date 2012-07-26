module ChordQual where

data ChordQual =
  CQM | CQm | CQdim | CQaug |
  CQ7 | CQM7 | CQm7 | CQdim7 | CQm7b5 | CQmM7 |
  CQaug7 | CQaugM7 | CQ9 | CQM9 | CQm9 | CQ7S9
  deriving (Eq, Ord, Show)

cqShow :: ChordQual -> String
cqShow = map (repl 'S' '+') . map (repl 'b' '-') . drop 2 . show

cqShowLong :: ChordQual -> String
cqShowLong CQM = "major"
cqShowLong CQm = "minor"
cqShowLong CQdim = "diminished"
cqShowLong CQaug = "augmented"
cqShowLong CQ7 = "dominant seventh"
cqShowLong CQM7 = "major seventh"
cqShowLong CQm7 = "minor seventh"
cqShowLong CQdim7 = "diminished seventh"
cqShowLong CQm7b5 = "half-diminished seventh"
cqShowLong CQmM7 = "minor major seventh"
cqShowLong CQaug7 = "augmented seventh"
cqShowLong CQaugM7 = "augmented major seventh"
cqShowLong CQ9 = "ninth"
cqShowLong CQM9 = "major ninth"
cqShowLong CQm9 = "minor ninth"
cqShowLong CQ7S9 = "dominant seventh sharp ninth (hendrix chord)"

repl :: Eq a => a -> a -> a -> a
repl targ r x = if x == targ then r else x
