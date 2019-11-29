module LevelReader where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Data.Text (Text)
import qualified Data.Text.IO as T

type Parser = Parsec Void Text

data Element = Wall | Box | Player | GoalSquare deriving (Show, Eq, Ord)
data Coord2D = Coord2D { x :: Int
                       , y :: Int
                       } deriving (Show, Eq, Ord)
newtype Level = Level [(Coord2D, Element)] deriving (Show, Eq, Ord)

wall :: Parser [Element]
wall = char '#' >> return [Wall]

box :: Parser [Element]
box = char '$' >> return [Box]

player :: Parser [Element]
player = char '@' >> return [Player]

goalSquare :: Parser [Element]
goalSquare = char '.' >> return [GoalSquare]

playerOnGoalSquare :: Parser [Element]
playerOnGoalSquare = char '+' >> return [Player, GoalSquare]

boxOnGoalSquare :: Parser [Element]
boxOnGoalSquare = char '*' >> return [Box, GoalSquare]


element :: Parser [Element]
element = wall <|> box <|> player <|> goalSquare <|> playerOnGoalSquare <|> boxOnGoalSquare

row :: Parser [(Int, Element)]
row = do
  raw <- manyTill ((Just <$> element) <|> (char ' ' >> return Nothing)) (char '\n')
  let countedRaw = zip [0..] raw
  return [ (i,e) | (i, Just es) <- countedRaw, e <- es ]

level :: Parser Level
level = do
  rows <- some row
  let countedRows = reverse . zip [0..] . reverse $ rows
  return $ Level [ (Coord2D x y, e) | (y, es) <- countedRows, (x, e) <- es ]

levelSep :: Parser ()
levelSep = do
  _ <- char ';'
  _ <- takeWhileP Nothing (/= '\n')
  _ <- char '\n'
  _ <- takeWhileP Nothing (== ' ')
  _ <- char '\n'
  return ()

levels :: Parser [Level]
levels = level `endBy` levelSep

readLevels :: FilePath -> IO [Level]
readLevels fPath = do
   cont <- T.readFile fPath
   let res = parse levels fPath cont
   case res of
     Left err -> error $ show err
     Right value -> return value
