import Data.Char
import Data.List
import Control.Monad

solve1 = sum . map auxiliary
  where
    auxiliary input = read [first_digit, last_digit] :: Int
      where
        Just first_digit = find isDigit input  
        Just last_digit = find isDigit $ reverse input

part1 path = solve1 . lines <$> readFile path

patterns =
  map show [1..9] ++
  ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

reversedPatterns = map reverse patterns

isMatch a b = and $ zipWith (==) a b

isPatternMatch patterns a = any (isMatch a) patterns 

findPattern patterns a = find (isMatch a) patterns

toNum "one" = '1'
toNum "two" = '2'
toNum "three" = '3'
toNum "four" = '4'
toNum "five" = '5'
toNum "six" = '6'
toNum "seven" = '7'
toNum "eight" = '8'
toNum "nine" = '9'
toNum [a] = a 

findThePattern p = findPattern p <=< (find (isPatternMatch p) . tails)

solve2 = sum . map auxiliary
  where
    auxiliary input = read $ map toNum [first_digit, last_digit] :: Int
      where
        Just first_digit = findThePattern patterns input
        Just last_digit =
          fmap reverse
          $ findThePattern reversedPatterns
          $ reverse input

part2 path = solve2 . lines <$> readFile path
