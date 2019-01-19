module Lib
    ( someFunc
    ) where

import Control.Applicative
import Control.Monad.State.Lazy
import Data.Foldable
import Data.List
import Data.Ratio
import Data.Tree
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Sequence as S
import qualified Data.Set as Set

data STree a  = SNode a (SForest a) deriving (Show, Ord, Eq)
type SForest a = S.Seq (STree a)

areaSym :: Ord a => a -> a -> a -> a -> [(M.Map a a, Int)]
areaSym a b c d = [(M.fromList [(a, b), (b, a)], -1),
                   (M.fromList [(c, d), (d, c)], -1),
                   (M.fromList [(a, c), (b, d), (c, a), (d, b)], 1)]

areaBlocksSym :: Ord a => a -> a -> a -> a ->
                          a -> a -> a -> a ->
                          [(M.Map a a, Int)]
areaBlocksSym a b c d e f g h =
    [(M.fromList [(a, e), (b, f), (c, g), (d, h),
                  (e, a), (f, b), (g, c), (h, d)], 1)]

sortPair :: Ord a => (a, a) -> (a, a)
sortPair (a, b) = if a > b then (b, a) else (a, b)

sortBranch :: Ord a => [(a, a)] -> [(a, a)]
sortBranch = sort . map sortPair

addVals :: (Num a, Eq b) => (a, b) -> (a, b) -> (a, b)
addVals (a, b) (c, d)
    | b == d = (a+c, b)
    | otherwise = undefined

multVal :: Num a => a -> (a, b) -> (a, b)
multVal f (a, b) = (f*a, b)

symmetrizeBranch :: (Ord a, Fractional b) => [(M.Map a a, Int)] -> Int -> [(a, a)] -> M.Map [(a, a)] (b, Int)
symmetrizeBranch syms l b = foldl' (\b' (m, s) -> M.unionsWith addVals $ M.foldMapWithKey (\k (a, l') -> [M.map (multVal a) $ symmetrizeBranch' m s l k]) b') (M.singleton b (1, l)) syms

symmetrizeBranch' :: (Ord a, Fractional b) => M.Map a a -> Int -> Int -> [(a, a)] -> M.Map [(a, a)] (b, Int)
symmetrizeBranch' m s l b = if b == b' then if s == -1 then M.empty
                                                       else M.singleton b (1, l)
                                       else M.fromList [(b, (fromRational (1 % 2), l)), (b', ((fromIntegral s) * (fromRational (1 % 2)), l))]
    where
       b' = sortBranch $ map (\(a, b) -> (M.findWithDefault a a m, M.findWithDefault b b m)) b 

symmetrizeBranches :: (Ord a, Fractional b) => [(M.Map a a, Int)] -> [[(a, a)]] -> M.Map [(a, a)] (b, Int)
symmetrizeBranches syms bs = snd $ execState (traverse (symmetrizeBranchAndInsert syms) bs) (1, M.empty)

symmetrizeBranchAndInsert :: (Ord a, Fractional b) => [(M.Map a a, Int)] -> [(a, a)] -> State ((Int, M.Map [(a, a)] (b, Int))) ()
symmetrizeBranchAndInsert syms b = state (\(ml, m) -> case M.lookup b m of
                                                        Nothing -> ((), (ml+1, M.union m $ symmetrizeBranch syms ml b))
                                                        Just _ -> ((), (ml, m)))

labelSet :: Ord a => M.Map [(a, a)] (b, Int) -> Set.Set Int
labelSet = Set.fromList . (map snd) . toList

labelMap :: Set.Set Int -> IM.IntMap Int
labelMap s = IM.fromList $ zip (Set.toList s) [1..]

relabelBranches :: Ord a => M.Map [(a, a)] (b, Int) -> M.Map [(a, a)] (b, Int)
relabelBranches bs = fmap (\(val, label) -> (val, lm IM.! label)) bs
    where
        lm = labelMap $ labelSet bs

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
          let a8 = ansatzEtas 8
          let bs = branches a8
          let sym = areaSym 'a' 'b' 'c' 'd' ++
                    areaSym 'e' 'f' 'g' 'h' ++
                    areaBlocksSym 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h'
          putStr $ unlines $ map show $ (M.assocs $ relabelBranches $ symmetrizeBranches sym bs :: [([(Char, Char)], (Rational, Int))])

someFunc :: IO ()
someFunc = main
