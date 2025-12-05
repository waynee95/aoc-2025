import Data.Char (digitToInt, isDigit)
import Data.List (maximumBy, tails)
import Data.Ord (comparing)
import Text.ParserCombinators.ReadP
import Text.Printf (printf)

type Bank = [Int]

newline :: ReadP Char
newline = char '\n'

parseBank :: ReadP Bank
parseBank = map digitToInt <$> munch1 isDigit

parseBanks :: ReadP [Bank]
parseBanks = sepBy parseBank newline

findMaxes :: Int -> [Int] -> [Int]
findMaxes n xs = take n . reverse $ go xs [] (length xs - n)
  where
    go [] acc _ = acc
    go (y : ys) [] k = go ys [y] k
    go (y : ys) (a : acc) k
        | k > 0 && y > a = go (y : ys) acc (k - 1)
        | otherwise = go ys (y : a : acc) k

toInt :: [Int] -> Int
toInt xs = read (concatMap show xs)

part1 :: [Bank] -> Int
part1 = sum . map (toInt . findMaxes 2)

part2 :: [Bank] -> Int
part2 = sum . map (toInt . findMaxes 12)

main :: IO ()
main = do
    input <- fst . last . readP_to_S parseBanks <$> readFile "input.txt"
    printf "Part 1: %d\n" (part1 input)
    printf "Part 2: %d\n" (part2 input)

-- Part 1: 17445
-- Part 2: 3121910778619
