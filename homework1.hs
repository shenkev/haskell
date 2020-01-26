{-# OPTIONS_GHC -Wall #-}


toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0        = []
    | otherwise     = mod n 10: toDigitsRev (quot n 10)


-- toDigits :: Integer -> [Integer]
-- toDigits n = reverse (toDigitsRev n)


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x: y: zs) = x : 2*y : doubleEveryOther zs


sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigitsRev x) + sumDigits xs


validate :: Integer -> Bool
validate list = mod (sumDigits (doubleEveryOther (toDigitsRev list))) 10 == 0


main :: IO ()
main = print (validate 4012888888881881)