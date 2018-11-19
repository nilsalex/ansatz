module PermOrbits where

import Data.List (sort,nub)
import Data.Foldable
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Data.Char
import qualified Math.Algebra.Group.PermutationGroup as PG
import qualified Math.Core.Utils as MCU
import System.IO

permuteIndices :: PG.Permutation Int -> String -> String
permuteIndices perm = map (PG..^ charPerm)
    where charPerm = PG.fmapP (chr . (+ 96)) perm

permFromIndices :: String -> PG.Permutation Int
permFromIndices indices = PG.fromList positions
    where numbers = map ((+ (-96)) . ord) indices
          positions = map snd $ sort $ zip numbers [1..]

{- given a left group and a right group, yields the
   "orbit" by acting with all elements from the left
   and from the right, respectively -}
lrOrbit :: PG.Permutation Int -> [PG.Permutation Int] -> [PG.Permutation Int] -> S.Set (PG.Permutation Int)
lrOrbit groupElem leftGroup rightGroup = S.fromList $ (\lp rp -> lp * groupElem * rp) <$> leftGroup <*> rightGroup

getPerm :: IO (PG.Permutation Int)
getPerm = fmap PG.fromCycles readLn

getInt :: IO Int
getInt = readLn

metricGroupLeft :: [PG.Permutation Int]
metricGroupLeft = PG.elts $ map PG.fromCycles
                [[[2, 3]], [[5, 6]], [[1, 4], [2, 5], [3, 6]]]

{- subgroup of the symmetric group which acts from the right,
   i.e. which permute the values of indices -}
areaGroupLeft :: [PG.Permutation Int]
areaGroupLeft = PG.elts $ map PG.fromCycles
                [[[3, 4]], [[5, 6]], [[3, 5], [4, 6]],
                 [[9, 10]], [[11, 12]], [[9, 11], [10, 12]],
                 [[1, 2]], [[7, 8]],
                 [[1, 7], [2, 8], [3, 9], [4, 10], [5, 11], [6, 12]]]

metricGroupRight :: [PG.Permutation Int]
metricGroupRight = PG.elts $ map PG.fromCycles
                [[[1, 2]], [[3, 4]], [[5, 6]], [[1, 3], [2, 4]], [[1, 5], [2, 6]], [[3, 5], [4, 6]]]

{- subgroup of the symmetric group which acts from the left,
   i.e. which permute the positions of indices -}
areaGroupRight :: [PG.Permutation Int]
areaGroupRight = PG.elts $ map PG.fromCycles
                [[[1, 2]], [[3, 4]], [[1, 3], [2, 4]],
                 [[5, 6]], [[7, 8]], [[5, 7], [6, 8]],
                 [[9, 10]], [[11, 12]], [[9, 11], [10, 12]],
                 [[1, 5], [2, 6], [3, 7], [4, 8]],
                 [[1, 9], [2, 10], [3, 11], [4, 12]],
                 [[5, 9], [6, 10], [7, 11], [8, 12]]]

{- given a group of left permutations, a group of right permutations,
   and a sequence of input permutations,
   yield representatives of the quotient
   input permutations / left and right action of left and right permutation group -}
uniques :: [PG.Permutation Int] -> [PG.Permutation Int] ->
           Seq.Seq (PG.Permutation Int) -> [(PG.Permutation Int, Int)]
uniques gl gr right
    | Seq.length right < 2 = zip (toList right) $ repeat 0
    | otherwise        =
        let perm = right `Seq.index` 0
            orbit' = lrOrbit perm gl gr
            right' = Seq.filter (not . (`S.member` orbit')) $ Seq.deleteAt 0 right
            remaining = Seq.length right'
        in (perm, remaining) : uniques gl gr right'

{- same as uniques, but with additional counter of remaining permutations to check -}
uniques' :: [PG.Permutation Int] -> [PG.Permutation Int] ->
           Seq.Seq (PG.Permutation Int) -> [(PG.Permutation Int, Int)]
uniques' gl gr right
    | Seq.length right < 2 = zip (toList right) $ repeat 0
    | otherwise        =
        let perm = right `Seq.index` 0
            orbit' = lrOrbit perm gl gr
            right' = Seq.filter (not . (`S.member` orbit')) $ Seq.deleteAt 0 right
            remaining = Seq.length right'
        in (perm, remaining) : uniques gl gr right'
