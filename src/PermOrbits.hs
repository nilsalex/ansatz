module PermOrbits where

import Data.List (sort,nub)
import qualified Data.Set as S
import Data.Char
import qualified Math.Algebra.Group.PermutationGroup as PG
import qualified Math.Core.Utils as MCU
import System.IO

permuteIndices :: PG.Permutation Int -> [Char] -> [Char]
permuteIndices perm = map (PG..^ charPerm)
    where charPerm = PG.fmapP (chr . (+ 96)) perm

permFromIndices :: [Char] -> PG.Permutation Int
permFromIndices indices = PG.fromList positions
    where numbers = map ((+ (-96)) . ord) indices
          positions = map snd $ sort $ zip numbers [1..]

lrOrbit :: PG.Permutation Int -> [PG.Permutation Int] -> [PG.Permutation Int] -> S.Set (PG.Permutation Int)
lrOrbit groupElem leftGroup rightGroup = S.fromList $ (\lp rp -> lp * groupElem * rp) <$> leftGroup <*> rightGroup

getPerm :: IO (PG.Permutation Int)
getPerm = fmap PG.fromCycles readLn

getInt :: IO (Int)
getInt = readLn

metricGroupLeft :: [PG.Permutation Int]
metricGroupLeft = PG.elts $ map PG.fromCycles
                [[[2, 3]], [[5, 6]], [[1, 4], [2, 5], [3, 6]]]

areaGroupLeft :: [PG.Permutation Int]
areaGroupLeft = PG.elts $ map PG.fromCycles
                [[[3, 4]], [[5, 6]], [[3, 5], [4, 6]],
                 [[9, 10]], [[11, 12]], [[9, 11], [10, 12]],
                 [[1, 2]], [[7, 8]],
                 [[1, 7], [2, 8], [3, 9], [4, 10], [5, 11], [6, 12]]]

metricGroupRight :: [PG.Permutation Int]
metricGroupRight = PG.elts $ map PG.fromCycles
                [[[1, 2]], [[3, 4]], [[5, 6]], [[1, 3], [2, 4]], [[1, 5], [2, 6]], [[3, 5], [4, 6]]]

areaGroupRight :: [PG.Permutation Int]
areaGroupRight = PG.elts $ map PG.fromCycles
                [[[1, 2]], [[3, 4]], [[1, 3], [2, 4]],
                 [[5, 6]], [[7, 8]], [[5, 7], [6, 8]],
                 [[9, 10]], [[11, 12]], [[9, 11], [10, 12]],
                 [[1, 5], [2, 6], [3, 7], [4, 8]],
                 [[1, 9], [2, 10], [3, 11], [4, 12]],
                 [[5, 9], [6, 10], [7, 11], [8, 12]]]

{-
counts :: Int -> [PG.Permutation Int] -> [PG.Permutation Int] -> [[Char]] -> [Int]
counts len gl gr indicesList = map (\indices -> length . filter (indices `elem`) $ orbits') indicesList
    where orbits' = map (\indices ->
                          map (\perm -> permuteIndices perm $ take len ['a','b'..]) $
                          lrOrbit (permFromIndices indices) gl gr)
                        indicesList

uniques' :: Int -> [PG.Permutation Int] -> [PG.Permutation Int] -> [[Char]] -> [[Char]] -> [[Char]]
uniques' len gl gr us is = us ++ filter (not . (`elem` orbits')) is
    where orbits' = concat $
                      map (\indices ->
                          map (\perm -> permuteIndices perm $ take len ['a','b'..]) $
                          lrOrbit (permFromIndices indices) gl gr)
                        us

uniques :: Int -> [PG.Permutation Int] -> [PG.Permutation Int] -> [[Char]] -> [[Char]]
uniques _ _ _ (i:[]) = i:[]
uniques len gl gr (i:is) = uniques' len gl gr [i] $ uniques len gl gr is

metricUniques :: [[Char]] -> [[Char]]
metricUniques = uniques 6 metricGroupLeft metricGroupRight

areaUniques :: [[Char]] -> [[Char]]
areaUniques = uniques 12 areaGroupLeft areaGroupRight
-}
