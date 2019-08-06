module Main where

import PermOrbits
import Topsorts
import System.IO

import Data.Foldable
import Data.List

import qualified Math.Algebra.Group.PermutationGroup as PG

import qualified Data.Set as S
import qualified Data.Sequence as Seq

import Control.Parallel.Strategies

intersection :: S.Set SignedPerm -> Int
intersection o =
         let (positive, negative) = S.partition (\(SignedPerm _ s) -> s > 0) o
             p' = S.map (\(SignedPerm p _) -> p) positive
             n' = S.map (\(SignedPerm p _) -> p) negative
             inter = S.intersection p' n'
         in S.size inter

nonZeros :: [SignedPerm] -> [SignedPerm] -> Int -> Seq.Seq SignedPerm -> [(Int, Int)]
nonZeros gl gr count perms
    | size == 0 = []
    | otherwise = (size - 1, count') : nonZeros gl gr count' (Seq.drop 1 perms)
    where size = Seq.length perms
          perm = perms `Seq.index` 0
          orbit = lrOrbit perm gl gr
          pred = intersection orbit == 0
          count' = if pred then count + 1 else count

delta :: Char -> Char -> String
delta a b = "\\delta^{" ++ [a] ++ "}_{" ++ [b] ++ "}"

cadabraAnsatz :: String -> String
cadabraAnsatz is = foldl' (\s (a, b) -> s ++ delta a b) "" zipped
    where zipped = zip is "mpijkl"

main :: IO ()
main = do
--         hSetBuffering stdout NoBuffering
 --        let sorts = sort chrSorts
--         putStr $ unlines $ map cadabraAnsatz sorts
{-
         let sorts' = toList $ fmap (\indices -> SignedPerm (permFromIndices indices) 1) sorts
         let gl = areaGroupLeft
         let gr = areaGroupRight
         let partials = map (evalChunk gl gr) chunks
         print $ sum $ toList counts
-}         
--         let sorts = areaSorts'
--         putStr $ unlines $ map show sorts
        print $ uniques areaGroupLeft areaGroupRight $ Seq.singleton $ PG.fromCycles []
