module HigherOrderFunction where

import Data.List
import Data.Char

add :: Num a => a -> a -> a
add x y = x + y 

twice :: (a -> a) -> a -> a 
twice f x = f (f x)

inc :: Num a => a -> a
inc = add 1

hoge ::  Num a => a -> a
hoge x = 2 * x + 1

sample :: (Num a, Enum a) => [a]
sample = [1 .. 10]

cons :: a -> [a] -> [a]
cons = (:)

snoc :: [a] -> a -> [a]
snoc = flip cons

sonomama :: [a] -> [a]
sonomama = foldr cons []

gyakumuki :: [a] -> [a]
gyakumuki = foldl snoc []

twice' :: (a -> a) -> (a -> a)
twice' f = f . f

type Bin =  Int

bin2int :: [Bin] -> Int
bin2int = foldr phi 0
    where
        phi b s = b + 2 * s

int2bin :: Int -> [Bin] 
int2bin = unfoldr phi       
    where
        phi 0 = Nothing
        phi n = case n `divMod` 2 of
            (q,r) -> Just (r,q)

make8 :: [Bin] -> [Bin]
make8 = take 8 . (++ repeat 0)

encode :: String -> [Bin]
encode = concatMap (make8 . int2bin . ord)

decode :: [Bin] -> String
decode = map (chr . bin2int) . chop8

chop8 :: [Bin] -> [[Bin]]
chop8 = unfoldr psi
    where
        psi [] = Nothing
        psi xs = Just (splitAt 8 xs)
