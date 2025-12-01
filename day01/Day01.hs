import Data.Char (isDigit)
import Text.ParserCombinators.ReadP
import Text.Printf (printf)

data Direction = L | R deriving (Show, Eq)
data Rotation = Rotation Direction Int deriving (Show, Eq)

newline :: ReadP Char
newline = char '\n'

parseInt :: ReadP Int
parseInt = read <$> munch1 isDigit

parseDirection :: ReadP Direction
parseDirection = (char 'L' >> return L) +++ (char 'R' >> return R)

parseRotation :: ReadP Rotation
parseRotation = Rotation <$> parseDirection <*> parseInt

parseRotations :: ReadP [Rotation]
parseRotations = sepBy parseRotation newline

rotate :: Int -> Rotation -> Int
rotate val (Rotation dir rot) = case dir of
    L -> (val - rot) `mod` 100
    R -> (val + rot) `mod` 100

part1 :: [Rotation] -> Int
part1 = length . filter (== 0) . scanl rotate 50

part2 :: [Rotation] -> Int
part2 = part1 . concatMap step
  where
    step (Rotation L n) = replicate n (Rotation L 1)
    step (Rotation R n) = replicate n (Rotation R 1)

main :: IO ()
main = do
    input <- fst . last . readP_to_S parseRotations <$> readFile "input.txt"
    printf "Part 1: %d\n" (part1 input)
    printf "Part 2: %d\n" (part2 input)

-- Part 1: 1055
-- Part 2: 6386
