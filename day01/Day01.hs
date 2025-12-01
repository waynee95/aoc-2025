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
parseRotation = do
    dir <- parseDirection
    dist <- parseInt
    return $ Rotation dir dist

parseRotations :: ReadP [Rotation]
parseRotations = sepBy parseRotation newline

rotate :: Int -> Rotation -> Int
rotate val (Rotation dir rot) = case dir of
    L -> (val - rot) `mod` 100
    R -> (val + rot) `mod` 100

part1 :: [Rotation] -> Int
part1 rotations = length . filter (== 0) $ scanl rotate 50 rotations

main :: IO ()
main = do
    input <- fst . last . readP_to_S parseRotations <$> readFile "input.txt"
    printf "Part 1: %d\n" (part1 input)

-- Part 1: 1055
