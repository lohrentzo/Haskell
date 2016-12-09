--import System.Random.Shuffle
import Data.List 

search :: Ord a => a -> [a] -> Bool
search x [] = False
search x l =
  if x < y
    then search x ys1
  else if x > y
    then search x ys2
  else True
  where
    ys1 = take half l
    (y:ys2) = drop half l
    half = length l `div` 2


sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:y:zs) = x <= y && sorted (y:zs)


permutation :: Eq a =>  [a] -> [a] -> Bool
permutation [] [] = True
permutation l1 l2 = 
  length l1 == length l2
  && searchall l1  l2


searchall :: Eq a => [a] -> [a] -> Bool
searchall [] l = True
searchall (x:xs) l =
  searchlinear x l && searchall xs l


searchlinear :: Eq a => a -> [a] -> Bool
searchlinear _ [] = False
searchlinear x (y:ys) = 
  x == y || searchlinear x ys


quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort l = quicksort' $ shuffle l


quicksort' :: Ord a => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = 
  (quicksort' lxs) ++ [x] ++ (quicksort' rxs)
  where
    lxs = [y | y<-xs, y<x] -- filter (<x) xs
    rxs = [y | y<-xs, y>=x] -- filter (>=x) xs

{--
shuffle [] = []
shuffle [x] = [x]
shuffle l = 
  last perms
--  drop ((length (perms)) -1) (perms) !! 0
  where
    perms = permutations l
--}
shuffle :: [a] -> [a]
shuffle = last . permutations


--pick :: [a] -> IO a
--pick xs = do
--    pos <- randomRIO (0, length xs - 1)
--    return (xs !! pos)


--insertionsort :: Ord a => [a] -> [a]
--insertionsort l = inserts l []


--inserts :: Ord a => [a] -> [a] -> [a]
--inserts [] l = l
--inserts (x:xs) l = inserts xs $ insert x l


--insert :: Ord a => a -> [a] -> [a]
--insert x [] = [x]
--insert x (y:ys) = 
--  if x <= y 
--    then x : y: ys
--  else y : insert x ys

{-
import System.Random
import Control.Applicative

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle lst = do
    (e, rest) <- pickElem <$> getIx
    (e:) <$> shuffle rest
    where
    getIx = getStdRandom $ randomR (1, length lst)
    pickElem n = case splitAt n lst of
        ([], s) -> error $ "failed at index " ++ show n -- should never match
        (r, s)  -> (last r, init r ++ s)
-}
