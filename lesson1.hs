sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = sumtorial(n-1)+n


hailstone n
    | mod n 2 == 0  = div n 2
    | otherwise     = 3*n + 1


nums, range, range2 :: [Integer]
nums = [1, 2, 3]
range = [1 .. 100]
range2 = [2, 4..10]
-- here it's deducing the pattern of adding by 2 each time
range3 = 3 : 2 : 99 : []
-- you're not allowed to postpend using this syntax, only prepend


hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)
-- order of these two definitions matter, you'll get an infinite list if you put the n case first


listLen :: [Integer] -> Integer
listLen []      = 0
listLen (x:xs)  = 1 + listLen xs
-- this syntax looks weird but we're doing pattern matching on the argument
-- it should look like (x:xs) where x is the first element, xs is the rest of the list
-- this length function takes O(N) which seems bad but it's actually efficient due to lazy evaluation
-- actually you are encouraged to write functions like these that iterate over entire data structures


sum_every_two :: [Integer] -> [Integer]
sum_every_two []              = []
sum_every_two [x]             = [x]
sum_every_two (x: y: zs)    = (x+y) : sum_every_two zs


main = print (sum_every_two [1, 2, 3, 4, 5])
-- main = mapM_ print [range, range2, range3]
-- it's fascinating you need to use a map just to do multiple prints
-- more about mapM and mapM_
-- https://hoogle.haskell.org/?hoogle=mapM
-- https://stackoverflow.com/questions/27609062/what-is-the-difference-between-mapm-and-mapm-in-haskell/27609146