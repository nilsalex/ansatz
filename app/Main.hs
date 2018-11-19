module Main where

import PermOrbits
import Topsorts
import System.IO

import qualified Data.Set as S
import qualified Data.Sequence as Seq

main :: IO ()
main = do
         hSetBuffering stdout NoBuffering
         let sorts = Seq.sort $ Seq.fromList areaSorts
         let sorts' = fmap permFromIndices sorts
         let gl = areaGroupLeft
         let gr = areaGroupRight
         let u = uniques' gl gr sorts'
--         print u
         print sorts
         putStr $ unlines $ map (\(p, i) -> "to check: " ++ show i ++ "\t" ++ permuteIndices p "abcdefghijkl") u
