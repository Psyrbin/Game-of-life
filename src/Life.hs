module Life where

import Graphics.Gloss
import Data.List(nub)
import Data.Maybe(fromJust)
import Graphics.Gloss.Interface.Pure.Game
import Stack
import Graphics.Gloss.Interface.IO.Game

data Cell = Cell
    { xc :: Float
    , yc :: Float
    , isAlive :: Bool
    }

instance Eq Cell where
    c1 == c2 = xc c1 == xc c2 && yc c1 == yc c2

type Field = [Cell]

data Game = Game 
    { field :: Field 
    , isPaused :: Bool
    , tillUpdate :: Float
    , updateTimer :: Float
    , prevFields :: Stack Field
    , mouseDown :: Bool
    }






initGame :: String -> Game
initGame str = Game (getField(words str)) False 0.1 0.1 Empty False

updateField :: Field -> Field
updateField f = nub $ removeDead (updateLiving f f ++ updateNeighbours f f)

updateNeighbours :: Field -> Field -> Field
updateNeighbours _ [] = []
updateNeighbours f (x:xs) = map (processDead f) (getNeighbours x) ++ updateNeighbours f xs


updateLiving :: Field -> Field -> Field
updateLiving _ [] = []
updateLiving f (x:xs) = [processAlive f x] ++ updateLiving f xs

removeDead :: Field -> Field
removeDead f = filter check f 
    where
        check (Cell _ _ p) = p

getNeighbours :: Cell -> [Cell]
getNeighbours = getN1 (-1) (-1)
    where
        getN1 0 0 c = getN1 1 0 c
        getN1 1 1 c = [Cell (xc c + 1) (yc c + 1) False]
        getN1 1 j c = [Cell (xc c + 1) (yc c + 1 * j) False] ++ getN1 (-1) (j + 1) c
        getN1 i j c = [Cell (xc c + 1 * i) (yc c + 1 * j) False] ++ getN1 (i + 1) (j) c

processDead :: Field -> Cell -> Cell
processDead f c = Cell (xc c) (yc c) ((numOfSame (getNeighbours c) f) == 3)


processAlive :: Field -> Cell -> Cell
processAlive f c = Cell (xc c) (yc c) (aliveNeighbours == 3 || aliveNeighbours == 2)
    where
        aliveNeighbours = (numOfSame (getNeighbours c) f)


numOfSame :: Eq a => [a] -> [a] -> Int
numOfSame [] _ = 0
numOfSame _ [] = 0
numOfSame (x:xs) f 
    | elem x f = 1 + numOfSame xs f
    | otherwise = numOfSame xs f









window :: Display
window = InWindow "Wndw" (round windowSize, round windowSize) (100, 100)

makeLineCoords :: Float -> Float -> Float -> [Path]
makeLineCoords x y num = horLineCoords y num ++ vertLineCoords x num

horLineCoords :: Float -> Float -> [Path]
horLineCoords _ 0 = []
horLineCoords y num = [[(-windowSize, y), (windowSize, y)]] ++ horLineCoords (y + cellSize) (num - 1)

vertLineCoords :: Float -> Float -> [Path]
vertLineCoords _ 0 = []
vertLineCoords x num = [[(x, -windowSize), (x, windowSize)]] ++ vertLineCoords (x + cellSize) (num - 1)

makeGrid :: [Path] -> [Picture]
makeGrid [] = []
makeGrid (x:xs) = [line x] ++ makeGrid xs

drawField :: Game -> IO Picture
drawField (Game f _ _ _ _ _) = return $ color black $ pictures (map drawCell f ++ makeGrid (makeLineCoords (-windowSize) (-windowSize) (windowSize * 2 / cellSize)))

drawCell :: Cell -> Picture
drawCell c = translate (xc c * cellSize + cellSize / 2) (yc c * cellSize + cellSize / 2) (rectangleSolid cellSize cellSize)

background :: Color
background = white



roundCell :: Float -> Float
roundCell a
    | a > 0 = fromIntegral(quot (round (a) :: Int) (round cellSize :: Int))
    | otherwise = fromIntegral(quot (round (a) :: Int) (round cellSize :: Int) - 1)




onEvent :: Event -> Game -> IO Game
onEvent (EventMotion (x, y)) game 
    | mouseDown game = return $ Game ((field game) ++ [Cell (roundCell x) (roundCell y) True]) (isPaused game) (tillUpdate game) (updateTimer game) (prevFields game) True
    | otherwise = return game
onEvent (EventKey (SpecialKey KeySpace) Down _ _) game = return $ Game (field game) (not (isPaused game)) (tillUpdate game) (updateTimer game) (prevFields game) (mouseDown game)
onEvent (EventKey (SpecialKey KeyUp) Down _ _) game = return $ Game (field game) (isPaused game) (tillUpdate game) (updateTimer game - 0.01) (prevFields game) (mouseDown game)
onEvent (EventKey (SpecialKey KeyDown) Down _ _) game = return $ Game (field game) (isPaused game) (tillUpdate game) (updateTimer game + 0.01) (prevFields game) (mouseDown game)
onEvent (EventKey (SpecialKey KeyRight) Down _ _) game = return $ Game (updateField $ field game) (isPaused game) (tillUpdate game) (updateTimer game ) (push (prevFields game) (field game) ) (mouseDown game)
onEvent (EventKey (SpecialKey KeyLeft) Down _ _) game 
    | isEmpty (prevFields game) = return $ game
    | otherwise = return $ Game (fromJust(snd $ pop(prevFields game))) (isPaused game) (tillUpdate game) (updateTimer game) (fst $ pop(prevFields game)) (mouseDown game)
onEvent (EventKey (MouseButton LeftButton) Down _ (x,y)) game = return $ Game ((field game) ++ [Cell (roundCell x) (roundCell y) True]) (isPaused game) (tillUpdate game) (updateTimer game) (prevFields game) True
--    where
--        roundCell :: Float -> Float
--       roundCell a
--            | a > 0 = fromIntegral(quot (round (a) :: Int) (round cellSize :: Int))
--            | otherwise = fromIntegral(quot (round (a) :: Int) (round cellSize :: Int) - 1)
onEvent (EventKey (MouseButton LeftButton) Up _ (x,y)) game = return $ Game ((field game) ++ [Cell (roundCell x) (roundCell y) True]) (isPaused game) (tillUpdate game) (updateTimer game) (prevFields game) False
onEvent (EventKey (Char 's') Down _ _) game = saveField game
onEvent (EventKey (Char 'l') Down _ _) game = loadGame game
onEvent _ g = return $ g


oneIter :: Float -> Game -> IO Game
oneIter _ (Game f True tu ut pf md) = return $ Game f True tu ut pf md
oneIter t (Game f False tu ut pf md) 
    | tu <= 0 = return $ Game (updateField f) False (ut - t) ut (push pf f) md
    | otherwise = return $ Game f False (tu - t) ut pf md

run :: Game -> IO ()
run game = playIO window white 30 game drawField onEvent oneIter







getField :: [String] -> Field
getField [] = []
getField (cell:rest) = [Cell x y True] ++ getField rest
    where
        pair = read cell :: (Float, Float)
        x = fst pair
        y = snd pair

saveField :: Game -> IO Game
saveField g = do writeFile "scenes/save" (fieldToStr (field g))
                 return g
    where
        fieldToStr (cell:xs) = "(" ++ show (xc cell) ++ "," ++ show (yc cell) ++ ") " ++ fieldToStr xs
        fieldToStr [] = []


loadGame :: Game -> IO Game
loadGame g = do str <- readFile "scenes/save"
                return $ Game (getField(words str)) (isPaused g) (tillUpdate g) (updateTimer g) (prevFields g) (mouseDown g)

windowSize :: Float
windowSize = 1000

fieldSize :: Float
fieldSize = 50

cellSize :: Float
cellSize =  windowSize / fieldSize