module Main where

import PermOrbits
import Topsorts
import System.IO

import qualified Data.Set as S
import qualified Data.Sequence as Seq

main :: IO ()
main = do
         hSetBuffering stdout NoBuffering
         let sorts = fmap permFromIndices $ Seq.sort $ Seq.fromList areaSorts
         let gl = areaGroupLeft
         let gr = areaGroupRight
         let sorts = fmap permFromIndices $ Seq.sort $ Seq.fromList areaSorts
         let u = uniques' gl gr sorts
         putStr $ unlines $ map (\(p, i) -> "to check: " ++ show i ++ "\t" ++ permuteIndices p "abcdef") u
