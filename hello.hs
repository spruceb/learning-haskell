import Data.List

-- fizzbuzzTransform :: Int -> String
-- fizzbuzzTransform i
--   | i `mod` 15 == 0 = "FizzBuzz"
--   | i `mod` 3 == 0 = "Fizz"
--   | i `mod` 5 == 0 = "Buzz"
--   | otherwise = show i


fizzbuzzTransform i
  | null string = show i
  | otherwise = string
  where ifDivis k s = if i `mod` k == 0 then s else ""
        string = (ifDivis 3 "Fizz") ++ (ifDivis 5 "Buzz")


main = putStr $ intercalate "\n" $ map fizzbuzzTransform [1..100]
