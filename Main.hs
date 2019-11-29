{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception (SomeException, catch)
import Control.Monad (foldM_, when)
import Control.Monad.Extra (firstJustM)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (foldl', traverse_)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, maybe)
import Data.Monoid (All (..))
import qualified Data.Text as Text
import Data.Text (Text)
import Foreign.C.Types (CInt (..))
import LevelReader (Coord2D (..), Element (..), Level (..), readLevels)
import Linear (V4 (..))
import qualified Paths_hsokoban as Paths
import SDL
import qualified SDL.Font as Font
import SDL.Image
import SDL.Input.Keyboard
import SDL.Vect (Point (..))
import System.Exit
import System.FilePath
import System.IO
import Text.Pretty.Simple

mkRect :: CInt -> CInt -> CInt -> CInt -> Rectangle CInt
mkRect x y w h = Rectangle (P (V2 x y)) (V2 w h)

data Block = Selector | Stone | Grass | Water | Princess | Rock | Star deriving (Eq, Ord, Show)

data Coord
  = Coord
      { x :: CInt,
        y :: CInt,
        z :: CInt
      }
  deriving (Eq, Show, Ord)

data World
  = World
      { width :: CInt,
        depth :: CInt,
        height :: CInt,
        content :: Map Coord [Block]
      }
  deriving (Eq, Show, Ord)

drawWorld :: Renderer -> RendererSize -> (Block -> Texture) -> World -> IO ()
drawWorld renderer rsize texture world =
  let heightInPixel = (height world - 1) * 80 + (depth world - 1) * 160
   in sequence_ $ do
        layer <- [0 .. height world - 1]
        column <- [0 .. width world - 1]
        row <- reverse [0 .. depth world - 1]
        case Map.lookup (Coord column row layer) (content world) of
          Nothing -> return $ return ()
          Just xs ->
            return $
              traverse_
                ( \bt ->
                    copy
                      renderer
                      (texture bt)
                      Nothing
                      ( Just $
                          toRenderer
                            ( WorldCoords $
                                Rectangle (P (V2 (200 * column) (heightInPixel - layer * 80 - row * 160))) (V2 214 354)
                            )
                      )
                )
                xs
  where
    toRenderer r =
      let (RendererCoords r2) = testtesttest (worldSize world) r rsize
       in r2

worldSize :: World -> WorldSize
worldSize (World w d h _) = WorldSize $ V2 (w * 214) (354 + (d -1) * 160 + (h -1) * 80)

newtype RendererSize = RendererSize (Rectangle CInt)

newtype WorldSize = WorldSize (V2 CInt)

newtype RendererCoords = RendererCoords (Rectangle CInt)

newtype WorldCoords = WorldCoords (Rectangle CInt)

testtesttest :: WorldSize -> WorldCoords -> RendererSize -> RendererCoords
testtesttest
  (WorldSize (V2 wwidth wheight))
  (WorldCoords (Rectangle (P (V2 wx wy)) (V2 width height)))
  (RendererSize (Rectangle (P (V2 roffx roffy)) (V2 rwidth rheight))) =
    let scalex = fromIntegral rwidth / fromIntegral wwidth
        scaley = fromIntegral rheight / fromIntegral wheight
        sx = scale scalex wx
        sy = scale scaley wy
        swidth = scale scalex width
        sheight = scale scaley height
     in RendererCoords $ Rectangle (P (V2 (sx + roffx) (sy + roffy))) (V2 swidth sheight)
    where
      scale = myScale

myScale :: Double -> CInt -> CInt
myScale f i = truncate $ f * fromIntegral i

appLoop :: a -> (Event -> a -> IO a) -> IO ()
appLoop gs fun = do
  event <- waitEvent
  newGS <- case eventPayload event of
    KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ KeycodeQ _)) ->
      exitSuccess
    _ -> fun event gs
  appLoop newGS fun

levelElement2Block :: Element -> Block
levelElement2Block Wall = Stone
levelElement2Block Box = Rock
levelElement2Block Player = Princess
levelElement2Block GoalSquare = Selector

addToCoord :: Map Coord [Block] -> (Coord, Block) -> Map Coord [Block]
addToCoord m (c, b) = Map.alter doIt c m
  where
    doIt Nothing = Just [b]
    doIt (Just xs) = Just (sort $ b : xs)

removeFromCoord :: Map Coord [Block] -> (Coord, Block) -> Map Coord [Block]
removeFromCoord m (c, b) = Map.alter doIt c m
  where
    doIt Nothing = Nothing
    doIt (Just xs) = Just $ filter (/= b) xs

level2World :: Level -> World
level2World (Level xs) = World
  { width = levelWidth,
    depth = levelDepth,
    height = 2,
    content = foldl' addToCoord Map.empty $ [(Coord x y 0, Grass) | x <- [0 .. levelWidth - 1], y <- [0 .. levelDepth - 1]] ++ levelContent
  }
  where
    levelWidth = fromIntegral $ maximum [x | (Coord2D x _, _) <- xs] + 1
    levelDepth = fromIntegral $ maximum [y | (Coord2D _ y, _) <- xs] + 1
    levelContent = [(Coord (fromIntegral x) (fromIntegral y) 1, levelElement2Block e) | (Coord2D x y, e) <- xs]

data GameState
  = GameState
      { levelSet :: [Level],
        actualLevel :: Int,
        world :: World,
        moves :: Int
      }
  deriving (Eq, Ord, Show)

getPrincessPos :: Map Coord [Block] -> Coord
getPrincessPos = fst . head . filter (\(_, s) -> Princess `elem` s) . Map.toList

coordFree :: Map Coord [Block] -> Coord -> Bool
coordFree m c = case Map.lookup c m of
  Nothing -> True
  Just [] -> True
  Just [Selector] -> True
  _ -> False

coordBox :: Map Coord [Block] -> Coord -> Bool
coordBox m c = maybe False (Rock `elem`) (Map.lookup c m)

keypressEvent :: Keycode -> (GameState -> GameState) -> Event -> GameState -> GameState
keypressEvent kc fun event gs = case eventPayload event of
  (KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ kc2 _)))
    | kc2 == kc -> fun gs
    | otherwise -> gs
  _ -> gs

nextLevel :: Event -> GameState -> GameState
nextLevel = keypressEvent KeycodeN $ \gs@GameState {..} ->
  if actualLevel + 1 < length levelSet
    then GameState
      { levelSet = levelSet,
        actualLevel = actualLevel + 1,
        world = level2World $ levelSet !! (actualLevel + 1),
        moves = 0
      }
    else gs

prevLevel :: Event -> GameState -> GameState
prevLevel = keypressEvent KeycodeP $ \gs@GameState {..} ->
  if actualLevel - 1 >= 0
    then GameState
      { levelSet = levelSet,
        actualLevel = actualLevel - 1,
        world = level2World $ levelSet !! (actualLevel - 1),
        moves = 0
      }
    else gs

reloadLevel :: Event -> GameState -> GameState
reloadLevel = keypressEvent KeycodeR $ \gs@GameState {..} ->
  GameState {world = level2World $ levelSet !! actualLevel, moves = 0, ..}

data Direction = Up | Down | Left | Right deriving (Eq, Show, Ord)

nextInDirection :: Direction -> Coord -> Coord
nextInDirection Up (Coord x y z) = Coord x (y + 1) z
nextInDirection Down (Coord x y z) = Coord x (y -1) z
nextInDirection Main.Left (Coord x y z) = Coord (x -1) y z
nextInDirection Main.Right (Coord x y z) = Coord (x + 1) y z

checkWin :: GameState -> Bool
checkWin GameState {..} = doIt world
  where
    doIt World {..} = getAll $ Map.foldMapWithKey checkField content
    checkField (Coord _ _ 0) _ = All True
    checkField _ e
      | Rock `elem` e && Selector `elem` e = All True
      | Rock `elem` e = All False
      | otherwise = All True

movePlayer :: Direction -> GameState -> GameState
movePlayer d gs@GameState {..} = gs {world = world {content = doIt}, moves = moves + doItMoves}
  where
    m = content world
    pPos = getPrincessPos m
    nPos = nextInDirection d pPos
    nnPos = nextInDirection d nPos
    movePart :: Block -> Coord -> Coord -> Map Coord [Block] -> Map Coord [Block]
    movePart b from to m = addToCoord (removeFromCoord m (from, b)) (to, b)
    movePrincess :: Map Coord [Block] -> Map Coord [Block]
    movePrincess = movePart Princess pPos nPos
    moveBox :: Map Coord [Block] -> Map Coord [Block]
    moveBox = movePart Rock nPos nnPos
    doIt :: Map Coord [Block]
    doIt = case (coordFree m nPos, coordBox m nPos, coordFree m nnPos) of
      (True, _, _) -> movePrincess m
      (False, True, True) -> moveBox . movePrincess $ m
      _ -> m
    doItMoves :: Int
    doItMoves = case (coordFree m nPos, coordBox m nPos, coordFree m nnPos) of
      (True, _, _) -> 1
      (False, True, True) -> 1
      _ -> 0

up, down, left, right :: Event -> GameState -> GameState
up = keypressEvent KeycodeUp (movePlayer Up)
down = keypressEvent KeycodeDown (movePlayer Down)
left = keypressEvent KeycodeLeft (movePlayer Main.Left)
right = keypressEvent KeycodeRight (movePlayer Main.Right)

renderWin :: Font.Font -> IO Surface
renderWin font = Font.blended font (V4 255 255 0 255) "WIN!"

getRendererSize :: WorldSize -> CInt -> CInt -> CInt -> RendererSize
getRendererSize (WorldSize (V2 wwidth wheight)) woffx width height =
  let scalex = fromIntegral width / fromIntegral wwidth
      scaley = fromIntegral height / fromIntegral wheight
      scale = min scalex scaley
      cwidth = myScale scale wwidth
      cheight = myScale scale wheight
      woffx2 = abs (cwidth - width) `div` 2
      woffy2 = abs (cheight - height) `div` 2
   in RendererSize (Rectangle (P (V2 (woffx + woffx2) woffy2)) (V2 cwidth cheight))

renderInfo :: Font.Font -> Renderer -> Int -> Int -> Int -> Rectangle CInt -> IO ()
renderInfo font renderer level levelCount moves (Rectangle (P (V2 x y)) (V2 w h)) = do
  let lines = ["Level", Text.pack (show level), "von", Text.pack (show levelCount), "Züge", Text.pack (show moves)]
  let lineHeight = h `div` fromIntegral (length lines)
  foldM_ (\acc line -> renderLine line acc lineHeight >> return (acc + 1)) 0 lines
  where
    renderLine :: Text -> Int -> CInt -> IO ()
    renderLine line lineNumber lineHeight = do
      levelS <- Font.blended font (V4 255 255 255 255) line
      sD <- surfaceDimensions levelS
      t <- createTextureFromSurface renderer levelS
      copy renderer t Nothing (Just $ centerRectangle sD (mkRect x (y + fromIntegral lineNumber * lineHeight) w lineHeight))

centerRectangle
  (V2 sw sh)
  (Rectangle (P (V2 tx ty)) (V2 tw th)) =
    let scalex = fromIntegral tw / fromIntegral sw
        scaley = fromIntegral th / fromIntegral sh
        scale = minimum [scalex, scaley, 1]
        scaledx = myScale scale sw
        scaledy = myScale scale sh
        wx = (tw - scaledx) `div` 2
        wy = (th - scaledy) `div` 2
     in mkRect (tx + wx) (ty + wy) scaledx scaledy

myPPrint :: (Show a, MonadIO m) => a -> m ()
myPPrint = pPrintNoColor

tryRenderDriver :: Window -> (CInt, RendererInfo) -> IO (Maybe Renderer)
tryRenderDriver window (idx, info) =
  catch
    ( do
        putStrLn "trying driver:"
        myPPrint info
        renderer <- createRenderer window idx defaultRenderer
        return (Just renderer)
    )
    ( \e -> do
        putStrLn ("couldn't load driver number " ++ show idx ++ ":")
        myPPrint (e :: SomeException)
        return Nothing
    )

getRenderer :: Window -> IO Renderer
getRenderer window = do
  drivers <- zip [0 ..] <$> getRenderDriverInfo
  maybeRenderer <- firstJustM (tryRenderDriver window) drivers
  case maybeRenderer of
    (Just renderer) -> return renderer
    Nothing -> do
      hPutStrLn stderr ("None of " ++ show (length drivers) ++ " was loadable.")
      exitFailure

main :: IO ()
main = do
  initializeAll
  Font.initialize
  window <- createWindow "planetcutetest" defaultWindow {windowResizable = True}
  renderer <- getRenderer window
  levels <- readLevels "levels.txt"
  datapath <- Paths.getDataDir
  stonetexture <- loadTexture renderer (datapath </> "images" </> "stoneblock.png")
  grasstexture <- loadTexture renderer (datapath </> "images" </> "grassblock.png")
  watertexture <- loadTexture renderer (datapath </> "images" </> "waterblock.png")
  rocktexture <- loadTexture renderer (datapath </> "images" </> "rock.png")
  selectortexture <- loadTexture renderer (datapath </> "images" </> "selector.png")
  startexture <- loadTexture renderer (datapath </> "images" </> "yellow-star.png")
  princess <- loadTexture renderer (datapath </> "images" </> "Princess.png")
  font <- Font.load (datapath </> "fonts" </> "overpass-extrabold.otf") 96
  -- :levelRenderTexture <- createTexture renderer RGB888 TextureAccessTarget (V2 x y)
  let startGameState = GameState {levelSet = levels, actualLevel = 0, world = level2World $ head levels, moves = 0}
  (V2 width height) <- get $ windowSize window
  appLoop startGameState $ \event gs' -> do
    let gs = foldl' (flip ($)) gs' (map ($ event) [nextLevel, prevLevel, reloadLevel, up, down, right, left]) -- way to clever and ugly
    (V2 wwidth wheight) <- get $ windowSize window
    let rsize = getRendererSize (worldSize (world gs)) (myScale (1 / 4) wwidth) (myScale (3 / 4) wwidth) wheight
    clear renderer
    rendererDrawColor renderer $= V4 255 0 0 255
    renderInfo
      font
      renderer
      (actualLevel gs)
      (length (levelSet gs))
      (moves gs)
      (mkRect 0 0 (myScale (1 / 4) wwidth) wheight)
    rendererDrawColor renderer $= V4 0 0 0 255
    drawWorld
      renderer
      rsize
      ( \case
          Stone -> stonetexture
          Water -> watertexture
          Grass -> grasstexture
          Princess -> princess
          Rock -> rocktexture
          Star -> startexture
          Selector -> selectortexture
      )
      (world gs)
    when (checkWin gs) $ do
      s <- renderWin font
      t <- createTextureFromSurface renderer s
      copy renderer t Nothing Nothing
    present renderer
    return gs
