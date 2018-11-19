module Topsorts
    ( metricSorts, areaSorts
    ) where

import Data.Char
import Data.List
import qualified Data.IntMap.Strict as IM

metricSorted :: Ord a => [a] -> Bool
metricSorted [i1, i2, i3, i4, i5, i6] = i2 < i3 && i5 < i6 && i1 < i4

areaSorted :: Ord a => [a] -> Bool
areaSorted [i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12] =
    i1 < i2 && i7 < i8 && i3 < i4 && i5 < i6 && i3 < i5 && i9 < i10 && i11 < i12 && i9 < i11 && i1 < i7

metricCanonical :: [Int] -> Bool
metricCanonical xs = i1 < i2 && i3 < i4 && i5 < i6 && i1 < i3 && i1 < i5 && i3 < i5
    where imap = IM.fromList $ zip xs [0..]
          i1 = imap IM.! 1
          i2 = imap IM.! 2
          i3 = imap IM.! 3
          i4 = imap IM.! 4
          i5 = imap IM.! 5
          i6 = imap IM.! 6

areaCanonical :: [Int] -> Bool
areaCanonical xs = i1 < i2 && i3 < i4 && i5 < i6 && i7 < i8 && i9 < i10 && i11 < i12 &&
               i1 < i3 && i5 < i7 && i9 < i11 &&
               i1 < i5 && i1 < i9 && i5 < i9
    where imap = IM.fromList $ zip xs [0..]
          i1 = imap IM.! 1
          i2 = imap IM.! 2
          i3 = imap IM.! 3
          i4 = imap IM.! 4
          i5 = imap IM.! 5
          i6 = imap IM.! 6
          i7 = imap IM.! 7
          i8 = imap IM.! 8
          i9 = imap IM.! 9
          i10 = imap IM.! 10
          i11 = imap IM.! 11
          i12 = imap IM.! 12

isAreaPair :: Int -> Int -> Bool
isAreaPair a b = diff == 1 && modulus == 1
    where diff = abs (a - b)
          minIndex = min a b
          modulus = minIndex `mod` 2

isAreaPerm :: Int -> Int -> Int -> Int -> Bool
isAreaPerm a b c d = modulus == 1 && sorted == [0, 1, 2, 3]
    where indices = sort [a, b, c, d]
          minIndex = minimum indices
          modulus = minIndex `mod` 4
          sorted = map (\i -> i - minIndex) indices

areaZero :: [Int] -> Bool
areaZero [i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12] =
    isAreaPair i1 i2 ||
    isAreaPair i7 i8 ||
    isAreaPerm i1 i2 i3 i4 ||
    isAreaPerm i1 i2 i5 i6 ||
    isAreaPerm i1 i2 i9 i10 ||
    isAreaPerm i1 i2 i11 i12 ||
    isAreaPerm i7 i8 i3 i4 ||
    isAreaPerm i7 i8 i5 i6 ||
    isAreaPerm i7 i8 i9 i10 ||
    isAreaPerm i7 i8 i11 i12

topsorts :: Int -> ([Int] -> Bool) -> [[Int]]
topsorts len sorted = filter sorted . permutations . take len $ [1..]

metricSorts :: [String]
metricSorts = map (map (chr . (96+))) . filter metricCanonical $ topsorts 6 metricSorted

areaSorts :: [String]
areaSorts = map (map (chr . (96+))) . filter (not . areaZero) . filter areaCanonical $ topsorts 12 areaSorted
