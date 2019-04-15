import Control.Monad.State
import System.Environment

type NodeCoord = (Int, Int)
type Node = (NodeType, NodeCoord)
type Graph = ([NodeCoord], Int, Int, NodeCoord, NodeCoord)
type Result = ([NodeCoord], Int, Bool)

data NodeType = Free Int
              | Wall
              deriving (Eq, Show)

data Direction = NUp
               | NDown
               | NLeft
               | NRight
               deriving (Eq, Show)

main :: IO ()
main = do
  args <- getArgs
  if length args == 1
  then do
    string <- readFile $ head $ args
    let graph = createGraphS string
        result = floodR graph
        in drawGraphWithResult graph result
  else
    putStrLn "(!) Please provide filename of a labyrinth file as an argument."

addWallToGraph :: Graph -> NodeCoord -> Graph
addWallToGraph (wall, w, h, s, e) curr = (curr:wall, w, h, s, e)

setGraphWidth :: Graph -> Int -> Graph
setGraphWidth (wall, _, h, s, e) w = (wall, w, h, s, e)

setGraphHeight :: Graph -> Int -> Graph
setGraphHeight (wall, w, _, s, e) h = (wall, w, h, s, e)

setStart :: Graph -> NodeCoord -> Graph
setStart (wall, w, h, _, e) s = (wall, w, h, s, e)

setEnd :: Graph -> NodeCoord -> Graph
setEnd (wall, w, h, s, _) e = (wall, w, h, s, e)

isEmpty :: String -> Bool
isEmpty [] = True
isEmpty _ = False

createGraphR :: String -> Graph -> (Int, Int) -> NodeCoord -> Graph
createGraphR [] partial (_, h) _ = partial
createGraphR (l:ls) partial (w, h) (x, y)
  | l == '#'     = createGraphR ls (addWallToGraph partial (x, y)) (w + 1, h) (x + 1, y)
  | l == '\n'    = createGraphR ls (setGraphHeight (setGraphWidth partial w) ((getHeight partial) + 1)) (0, h + 1) (0, y + 1)
  | l == '.'     = createGraphR ls partial (w + 1, h) (x + 1, y)
  | l == 'S'     = createGraphR ls (setStart partial (x, y)) (w + 1, h) (x + 1, y)
  | l == 'E'     = createGraphR ls (setEnd partial (x, y)) (w + 1, h) (x + 1, y)
  | otherwise    = createGraphR ls partial (w + 1, h) (x + 1, y)

createGraphS :: String -> Graph
createGraphS string = createGraphR string ([], 0, 0, (0, 0), (0, 0)) (0, 0) (0, 0)

directionToNodeCoord :: Direction -> NodeCoord
directionToNodeCoord direction = case direction of
                                    NUp -> (0, -1)
                                    NDown -> (0, 1)
                                    NLeft -> (-1, 0)
                                    NRight -> (1, 0)

(>+) :: NodeCoord -> NodeCoord -> NodeCoord
(>+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2) 

isInWall :: Graph -> NodeCoord -> Bool
isInWall (wall, _, _, _, _) node = 0 < wallHits
  where wallHits = length
                 . filter (\wallnode -> node `nceq` wallnode)
                 $ wall

isInBounds :: Graph -> NodeCoord -> Bool
isInBounds (_, width, height, _, _) (x, y) = 0 <= x && x < width && 0 <= y && y < height

isValidMove :: Graph -> NodeCoord -> Bool
isValidMove graph node = isInBounds graph node && (True /= (isInWall graph node))

getNext :: Graph -> Result -> Direction -> Maybe Result
getNext graph ([], len, state) direction
  | isValidMove graph nextNode        = Just ([nextNode], 1, newState)
  | otherwise                         = Nothing
  where nextNode = getStartNode graph
        newState = nextNode `nceq` (getEndNode graph)
getNext graph ((curr:rest), len, state) direction
  | result                = Just (nextNode:(curr:rest), len + 1, newState)
  | otherwise             = Nothing
  where nextNode  = curr >+ (directionToNodeCoord direction)
        newState  = nextNode `nceq` (getEndNode graph)
        result    = isValidMove graph nextNode && ((nextNode `elem` (curr:rest)) /= True)

nceq :: NodeCoord -> NodeCoord -> Bool
nceq (x1, y1) (x2, y2) = x1 == x2 && y1 == y2

getStartNode :: Graph -> NodeCoord
getStartNode (_, _, _, start, _) = start

getEndNode :: Graph -> NodeCoord
getEndNode (_, _, _, _, end) = end

safeRes :: [Result] -> Maybe Result
safeRes [] = Nothing
safeRes (x:xs) = Just x

getMinPaths :: [Result] -> [Result]
getMinPaths [] = []
getMinPaths (x:xs) = getMinPaths' (x:xs) (getLength x)

getLength :: Result -> Int
getLength (_, len, _) = len

getMinPaths' :: [Result] -> Int -> [Result]
getMinPaths' [] _ = []
getMinPaths' (x:xs) minlen
  | getLength x <= minlen   = x : (getMinPaths xs)
  | otherwise               = getMinPaths xs

floodR :: Graph -> Maybe Result
floodR graph = floodR' graph ([], 0, False)

floodR' :: Graph -> Result -> Maybe Result
floodR' graph (path, len, True) = Just (path, len, True)
floodR' graph res = safeRes
                  . filter (\(path, len, state) -> state)
                  . map (\(Just x) -> x)
                  . filter (\next -> case next of
                                        Nothing -> False
                                        otherwise -> True)
                  . map (\next -> floodR' graph =<< next)
                  . map (\dir -> getNext graph res dir)
                  $ [NUp, NDown, NLeft, NRight]

getWidth :: Graph -> Int
getWidth (_, w, _, _, _) = w

getHeight :: Graph -> Int
getHeight (_, _, h, _, _) = h

drawForNode :: Graph -> Result -> NodeCoord -> IO ()
drawForNode (wall, _, _, start, end) (res, _, _) node
  | node `nceq` start   = putStr "S"
  | node `nceq` end     = putStr "E"
  | node `elem` wall    = putStr "#"
  | node `elem` res     = putStr "o"
  | otherwise           = putStr "."

drawStep :: Graph -> Result -> NodeCoord -> IO ()
drawStep graph res node
  | y == getHeight graph          = putStrLn ""
  | otherwise                     = do
                                if x < width
                                then do
                                  drawForNode graph res node
                                  drawStep graph res (x + 1, y)
                                else do
                                  putStrLn ""
                                  drawStep graph res (0, y + 1)
  where (x, y) = node
        width = getWidth graph
        (resnodes, _, _) = res

drawGraphWithResult :: Graph -> Maybe Result -> IO ()
drawGraphWithResult graph Nothing = drawGraphWithResult graph (Just ([], 0, False))
drawGraphWithResult graph (Just res) = drawStep graph res (0, 0)
