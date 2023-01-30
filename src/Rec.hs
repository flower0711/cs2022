module Rec where

myreverse :: [a] -> [a]
myreverse []     = []
myreverse (x:xs) = reverse xs ++ [x]

sample :: [int]
sample = [1 .. 10000000]