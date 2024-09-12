-- Exercise 1 -> We first find the digits of a number. Define the functions:
-- toDigits :: Integer -> [Integer]
-- toDigitsRev :: Integer -> [Integer]
--
-- toDigits should convert positive Integers to a list of digits. For 0 or negative inputs toDigits should return the empty list
-- toDigitsRev should do the same but with the digits reversed
--
-- example: toDigits 1234 == [1,2,3,4]
-- example: toDigitsRev 1234 = [4,3,2,1]
-- example: toDigits 0 == []
-- example: toDigitsRev (-17) == []

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | n <= 9 = [n]
  | otherwise = toDigits d ++ [r]
  where
    r = n `rem` 10
    d = n `div` 10

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse $ toDigits n

-- Exercise 2 -> Once we have the digits in the proper order, we need to double every other one
-- Define a function doubleEveryOther :: [Integer] -> [Integer]
-- Remember that double Ever other should double every other number beginning from the right. That is the second to last, fourth to last
--
-- example: doubleEveryOther [8,7,6,5] == [16,7,12,5]
-- example: doubleEveryOther [1,2,3] == [1,4,3]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse $ applyEveryOther (* 2) (reverse xs)

applyEveryOther :: (a -> a) -> [a] -> [a]
applyEveryOther f (x : y : xs) = x : f y : applyEveryOther f xs
applyEveryOther _ xs = xs

-- Exercise 3 -> The output of doubleEveryOther has a mix of one digit and two digit numbers. Define the function
--
-- sumDigits :: [Integer] -> Integer
--
-- to calculate the sum of all the digits
--
-- example: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22

sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ map (sum . toDigits) xs

-- Exercise 4 -> Define the function
--
-- validate :: Integer :: Bool
--
-- that indicates whether an Integer could be a valid credit card number. This will use all functions defined in the previous exercies
--
-- example: validate 4012888888881881 = True
-- example: validate 4012888888881882 = False
--
-- Step 1. Double the value of every second digit beginning from the right.
-- That is, the last digit is unchanged; the second-to-last digit is dou-
-- bled; the third-to-last digit is unchanged; and so on. For example,
-- [1,3,8,6] becomes [2,3,16,6]
--
-- Step 2. Add the digits of the doubled values and the undoubled dig-
-- its from the original number. For example, [2,3,16,6] becomes
-- 2+3+1+6+6 = 18
--
-- Step 3. Calculate the remainder when the sum is divided by 10. For the
-- above example, the remainder would be 8.
-- If the result equals 0, then the number is valid.

validate :: Integer -> Bool
validate creditCardNumber = sum `rem` 10 == 0
  where
    sum = sumDigits $ doubleEveryOther $ toDigits creditCardNumber
