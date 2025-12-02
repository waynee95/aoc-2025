import Control.Monad (void)
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP
import Text.Printf (printf)

data Range = Range Int Int deriving (Show)

parseInt :: ReadP Int
parseInt = read <$> munch1 isDigit

parseRange :: ReadP Range
parseRange = Range <$> parseInt <* char '-' <*> parseInt

parseRanges :: ReadP [Range]
parseRanges = sepBy parseRange (char ',')

hasEqualHalves :: Int -> Bool
hasEqualHalves n =
    let s = show n
        len = length s
        half = len `div` 2
        (first, second) = splitAt half s
     in even len && first == second

part1 :: [Range] -> Int
part1 = sum . concatMap (filter hasEqualHalves . rangeToList)
  where
    rangeToList (Range a b) = [a .. b]

part2 :: [Range] -> Int
part2 = undefined

main :: IO ()
main = do
    input <- fst . last . readP_to_S parseRanges <$> readFile "input.txt"
    printf "Part 1: %d\n" (part1 input)
    printf "Part 2: %d\n" (part2 input)

-- Part 1:
-- Part 2:
