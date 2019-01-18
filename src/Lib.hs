module Lib
    ( someFunc
    ) where

import Control.Applicative
import Control.Monad.State.Lazy
import Data.Foldable
import Data.List
import Data.Tree
import qualified Data.Sequence as S

data STree a  = SNode a (SForest a) deriving (Show, Ord, Eq)
type SForest a = S.Seq (STree a)

fromBranches :: Eq a => [[a]] -> SForest a
fromBranches = foldl' insertBranch S.empty

insertBranch :: Eq a => SForest a -> [a] -> SForest a
insertBranch f []      = f
insertBranch f (x:xs)
    | S.null f  = S.fromList [SNode x $ insertBranch S.empty xs]
    | otherwise = case S.findIndexL (\(SNode n ts) -> n == x) f of
                    Nothing  -> f S.|> (SNode x $ insertBranch S.empty xs)
                    Just pos -> S.adjust (\(SNode n ts) -> SNode n (insertBranch ts xs)) pos f

toLForest :: SForest a -> Forest a
toLForest = toList . fmap toLTree

toLTree :: STree a -> Tree a
toLTree (SNode n f) = Node n $ toLForest f

pairToList :: Forest (a, a) -> Forest [a]
pairToList = (fmap . fmap) (\(x, y) -> [x, y])

pick :: [a] -> [(a, [a])]
pick xs = [(y, ys ++ ys') | (ys, y : ys') <- zip <$> inits <*> tails $ xs]

label :: [a] -> State Int [Either a Int]
label xs = state (\l -> (map Left xs ++ [Right l], l+1))

labelBranches :: [[a]] -> [[Either a Int]]
labelBranches bs = evalState (traverse label bs) 0

--- build up tree by recursive application
--- of innerEtas
buildEtas :: [a] -> Forest (a, a)
buildEtas []     = []
buildEtas (x:xs) = let ys = map (\(y, zs) -> ((x, y), zs)) $ pick xs
                   in map (\(node, list) -> Node node $ buildEtas list) ys

--- flatten Tree to list of strings
treeToStrings :: Tree String -> [String]
treeToStrings (Node n []) = [n]
treeToStrings (Node n ts) = (++) <$> [n] <*> (forestToStrings ts)

forestToStrings :: Forest String -> [String]
forestToStrings ts = concat $ map treeToStrings ts

branches :: Forest a -> [[a]]
branches = concat . map branches'

branches' :: Tree a -> [[a]]
branches' (Node n []) = [[n]]
branches' (Node n ts) = (++) <$> [[n]] <*> (branches ts)

--- build ansatz tree from n indices
ansatzEtas :: Integer -> Forest (Char, Char)
ansatzEtas n
  | n < 0 = undefined
  | n `mod` 2 /= 0 = undefined
  | otherwise = buildEtas $ take (fromIntegral n) ['a'..'z']

main :: IO ()
main = do
          let a8 = ansatzEtas 18
          let bs = branches a8
          let bs' = labelBranches bs
          let sf = fromBranches bs'
          print $ length $ branches $ toLForest sf

someFunc :: IO ()
someFunc = main
