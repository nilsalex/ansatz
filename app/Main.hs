module Main where

import PermOrbits
import Topsorts
import System.IO

import qualified Data.Set as S

main :: IO ()
main = do
--        let as = areaSorts
--        putStrLn "Ansaetze from topological sorts (M ⊆ G):"
--        print $ as
--        putStrLn "Representatives of M/G:"
--        print $ areaUniques as
         hSetBuffering stdout NoBuffering
         let gl = areaGroupLeft
         let gr = areaGroupRight
         let sorts = areaSorts
         let sortsPerms = map permFromIndices sorts
         let counts = map (\sortPerm -> length $ filter (`S.member` (lrOrbit sortPerm gl gr)) sortsPerms) sortsPerms
         putStr $ unlines $ map show counts
