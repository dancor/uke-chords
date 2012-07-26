module ChordSig where

import ChordQual

data ChordSig = ChordSig {
  csSig :: [Int],
  csIntvlOrd :: Int,
  csQual :: ChordQual}
  deriving Show
