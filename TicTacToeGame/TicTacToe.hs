import Data.Foldable
import Data.List
import System.Process
import Control.Monad.State
import System.IO

main :: IO ()
main = do

    hideCursor

    hSetBuffering stdin NoBuffering
    hSetEcho stdout False

    drawAll
    waitForInput [] (PX0, PY0)
    
    showCursor
    clearScreen
    printStringOn (0,0) "Thank you for playing! Bye!\n" False

    return ()

programLoop :: IO ()
programLoop = do
  player <- choosePlayer
  drawAll
  waitForInput [] (PX0, PY0)
  return ()

        -- ****************************** GUI SETTINGS ******************************** --

gOrigin :: (Int, Int)
gOrigin = (10, 5)

gWidth :: Int
gWidth = 3

gHeight :: Int
gHeight = 3

gPaddingX :: Int
gPaddingX = 0

gPaddingY :: Int
gPaddingY = 0

        -- ******************************** GUI DATA ********************************** --

data TPosX = PX0 | PX1 | PX2 deriving (Eq, Show, Enum)
data TPosY = PY0 | PY1 | PY2 deriving (Eq, Show, Enum)

data Direction = DLeft | DRight | DUp | DDown deriving (Eq)

instance Show Direction where
  show DUp = "Up"
  show DDown = "Down"
  show DLeft = "Left"
  show DRight = "Right"

        -- ******************************** GUI ******************************** --

drawAll :: IO ()
drawAll = do

    clearScreen

    height <- drawBoard gOrigin gWidth gHeight gPaddingX gPaddingY
    
    logDown ("drawing board: O" ++ (show gOrigin) ++ " dim "
                                ++ (show $ length [(PX0)..]) ++ "×"
                                ++ (show $ length [(PY0)..]) ++ " each "
                                ++ (show gWidth) ++ "×"
                                ++ (show gHeight) ++ " pad "
                                ++ (show gPaddingX) ++ "×"
                                ++ (show gPaddingY)
                                ++ " (press ESC to quit)") (height + 1)

                                -- note that (height + 1) in logDown refers to
                                -- previous cursor position, so that log can
                                -- write on bottom of screen and then get
                                -- cursor back where it was - for now we don't
                                -- need this in our program...

clearScreen :: IO ()
clearScreen = putStrLn "\ESC[2J"

putChars :: Int -> Char -> String
putChars rep char
      | rep > 0         = char:putChars (rep - 1) char
      | otherwise       = ""

printBoxPart :: (Int, Int) -> Int -> Int -> Int -> Int -> Bool -> Int -> Bool -> IO Int
printBoxPart (x, y) width height padX padY isTopRow currLevel selected =

    if currLevel == height + 1 then return currLevel else

      if currLevel == 0 && selected && not isTopRow then do

        putStrLn $ "\ESC[" ++ (show (y+currLevel)) ++ ";" ++ (show x) ++ "H|\ESC[1m"
                                                   ++ (putChars (width * 2 - 1) wBound)
                                                   ++ "\ESC[m|"
        return =<< printBoxPart (x,y) width height padX padY isTopRow (currLevel + 1) selected

      else do
        
        printStringOn (x, (y+currLevel))
                      (hBound:(putChars (width * 2 - 1) wBound)
                   ++ [hBound]) selected
        return =<< printBoxPart (x,y) width height padX padY isTopRow (currLevel + 1) selected

    where hBound = if currLevel == 0 && ((padY /= 0) || isTopRow) then ' ' else '|'
          wBound = if currLevel `elem` [0, height] then '_' else ' '

sub :: (Int, Int) -> (Int, Int) -> (Int, Int)
(x1, y1) `sub` (x2, y2) = (x1 - x2, y1 - y2)

drawBox :: (Int, Int) -> Int -> Int -> Int -> Int -> Bool -> Bool -> IO Int
drawBox (x, y) width height padX padY isTopRow selected = do

                                                   newY <- printBoxPart (x, y) width height
                                                                        padX padY isTopRow 0
                                                                        selected
                                                   return (newY + y - 1)

printStringOn :: (Int, Int) -> String -> Bool -> IO Int
printStringOn (x, y) string bold = do putStrLn $ "\ESC[" ++ (show y) ++ ";" ++ (show x) ++ "H"
                                                         ++ escBoldStart ++ string ++ escBoldEnd
                                      return y

                                   where escBoldStart = if bold then "\ESC[1m" else ""
                                         escBoldEnd = if bold then "\ESC[m" else ""

screenPos :: (Int, Int) -> (TPosX, TPosY) -> Int -> Int -> Int -> Int -> (Int, Int)
screenPos (x, y) (posX, posY) width height padX padY = (x + padX + 1 + tPosX * (width * 2 + padX),
                                                        y + padY + tPosY * (height + padY))
                                         where tPosX = fromEnum posX
                                               tPosY = fromEnum posY
logDown :: String -> Int -> IO ()
logDown string prevY = do

    rows <- getRows
    printStringOn (0, rows - 1) ("\ESC[2K > \ESC[2m" ++ string ++ "\ESC[m") False
    putStrLn $ "\ESC[" ++ (show prevY) ++ ";0H"

getAllPositions :: [(TPosX, TPosY)]
getAllPositions = [ (a,b) | a <- [(PX0)..], b <- [(PY0)..] ]

drawBoard :: (Int, Int) -> Int -> Int -> Int -> Int -> IO Int
drawBoard (x, y) sWidth sHeight padX padY = return =<< foldlM (\acc curr -> do
    
  return =<< drawBox (screenPos (x, y) (curr) sWidth sHeight padX padY)
                     sWidth sHeight padX padY (isTopRow curr) False) 0 getAllPositions

isTopRow :: (TPosX, TPosY) -> Bool
isTopRow (_,PY0) = True
isTopRow _ = False

lengthWithoutEsc :: String -> Int
lengthWithoutEsc string = fst $ foldl (\acc x -> if x == '\ESC'
                                                 then
                                                   (fst acc, True)
                                                 else
                                                   if snd acc
                                                   then
                                                     (fst acc, not $ isLetter x)
                                                   else
                                                     (fst acc + 1, False))
                                      (0,False) string

isLetter :: Char -> Bool
isLetter char = char `elem` (['A'..'Z'] ++ ['a'..'z'])

        -- **************************** KEYBOARD **************************** --

waitForInput :: [Char] -> (TPosX, TPosY) -> IO ()
waitForInput prev pos = do
    
     x <- stdin `ifReadyDo` getChar
     takeAction x prev pos

move :: Direction -> (TPosX, TPosY) -> (Int, Int) -> Int -> Int
                  -> Int -> Int -> IO (TPosX, TPosY)
move dir pos origin width height padX padY = do

                                    logDown (show dir) 0

                                    drawBox realPos width height
                                            padX padY (isTopRow pos)
                                            False

                                    drawBox newPos width height
                                            padX padY (isTopRow new)
                                            True

                                    return new
        
        where realPos = screenPos gOrigin pos gWidth gHeight gPaddingX gPaddingY
              newPos = screenPos origin new width height padX padY
              new = (go pos dir)

moveConv :: Direction -> (TPosX, TPosY) -> IO (TPosX, TPosY)
moveConv dir pos = move dir pos gOrigin gWidth gHeight gPaddingX gPaddingY

takeAction :: Maybe Char -> [Char] -> (TPosX, TPosY) -> IO ()
takeAction Nothing prev pos

    | prev == ['\ESC']           = return ()
    | prev == ['A','[','\ESC']   = waitForInput [] =<< moveConv DUp pos
    | prev == ['B','[','\ESC']   = waitForInput [] =<< moveConv DDown pos
    | prev == ['C','[','\ESC']   = waitForInput [] =<< moveConv DRight pos
    | prev == ['D','[','\ESC']   = waitForInput [] =<< moveConv DLeft pos
    | prev == ['\n']             = do { logDown "Enter!" 0; drawCenterSymbol pos 'X'; waitForInput [] pos }
    | otherwise                  = waitForInput [] pos

takeAction (Just char) prev pos = waitForInput (char:prev) pos

ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo hnd x = hReady hnd >>= f
   where f True = x >>= return . Just
         f _    = return Nothing

incX :: TPosX -> TPosX
incX x = if x == last [(PX0)..] then x else succ x

incY :: TPosY -> TPosY
incY y = if y == PY0 then y else pred y

decX :: TPosX -> TPosX
decX x = if x == PX0 then x else pred x

decY :: TPosY -> TPosY
decY y = if y == last [(PY0)..] then y else succ y

go :: (TPosX, TPosY) -> Direction -> (TPosX, TPosY)
go (x, y) DUp = (x, incY y)
go (x, y) DRight = (incX x, y)
go (x, y) DLeft = (decX x, y)
go (x, y) DDown = (x, decY y)

        -- ***************************** TERMINAL ***************************** --

isNumeric :: Char -> Bool
isNumeric char = char `elem` "0123456789"

readFromTerminal :: String -> Int
readFromTerminal string = foldl (\acc x -> if isNumeric x
                                           then
                                             (acc * 10 + (read [x]))
                                           else acc) 0 string
getColumns :: IO Int
getColumns = do
     columnsRaw <- (readProcess "tput" ["cols"] [])
     let columns = readFromTerminal columnsRaw
     return columns

getRows :: IO Int
getRows = do
     rowsRaw <- (readProcess "tput" ["lines"] [])
     let rows = readFromTerminal rowsRaw
     return rows

hideCursor :: IO ()
hideCursor = callProcess "tput" ["civis"]

showCursor :: IO ()
showCursor = callProcess "tput" ["cnorm"]

        -- **************************** GAME SPECIFIC GUI ***************************** --

choosePlayer :: IO TPlay
choosePlayer = do
      player <- getLine
      return PL1

getSymbolForPlayer :: TPlay -> Char
getSymbolForPlayer n = case n of
                         PL1 -> 'X'
                         PL2 -> 'O'

drawCenterSymbol :: (TPosX, TPosY) -> Char -> IO ()
drawCenterSymbol tetrisPos symbol = do

  printStringOn (realX + (gWidth + 1) - 1, realY + (gHeight + 1) `div` 2) [symbol] False
  return ()

    where (realX, realY) = screenPos gOrigin tetrisPos gWidth gHeight gPaddingX gPaddingY

        -- ******************************** GAME DATA ********************************* --

data TPlay = PL1 | PL2 deriving (Eq, Show)

data TPlayType = Human | AI deriving (Eq, Show)

data TMove = TMove (TPosX, TPosY) TPlay | Dummy TPlay deriving (Eq, Show)

data TGameState = Empty | Node TMove ([TGameState]) deriving (Eq, Show)

        -- ******************************** GAME LOGIC ******************************** --

getPlayer :: TMove -> TPlay
getPlayer (TMove _ player) = player
getPlayer (Dummy player) = player

equalMovePositons :: TMove -> TMove -> Bool
(TMove pos1 _) `equalMovePositons` (TMove pos2 _) = pos1 == pos2

(>+) :: TMove -> (Maybe [TMove]) -> (Maybe [TMove])
move >+ Nothing = Nothing
move >+ (Just moves) = if move `elem` moves
                       then
                         Nothing
                       else
                         Just (move:moves)

-- add move as a new tree level --

(+>) :: TMove -> Maybe TGameState -> Maybe TGameState
newMove +> maybeState = case maybeState of
  
  Nothing      -> Nothing
  Just state   -> case state of

    Empty                     -> Just $ Node newMove []
    Node lastMove []          -> if getPlayer newMove == getPlayer lastMove
                                 || lastMove `equalMovePositons` newMove
                                 then
                                   Nothing
                                 else
                                   Just $ Node lastMove [Node newMove []]
    Node currentMove (x:xs)   -> case newMove +> (Just x) of
  
      Nothing         -> Nothing
      Just newState   -> if currentMove `equalMovePositons` newMove
                         then
                           Nothing
                         else
                           Just $ Node currentMove [newState]

switchPlayers :: TPlay -> TPlay
switchPlayers PL1 = PL2
switchPlayers PL2 = PL1

switchMove :: TMove -> TMove
switchMove (TMove pos play) = (TMove pos (switchPlayers play))

getPossibilities :: [TMove] -> [TMove]
getPossibilities moves = if isWin moves player || isWin moves (switchPlayers player) then [] else possibilities

        where possibilities = map (\pos -> TMove pos player)
                            . filter (\pos -> not $ pos `elem` (map (\x -> let (TMove pos _) = x in pos) moves))
                            $ getAllPositions

              player = if moves == []
                       then
                         PL1
                       else
                         let (x:_) = moves in switchPlayers $ getPlayer x

generateDecisionTree :: TGameState -> Int -> TGameState
generateDecisionTree state level
 -- starting player is PL1, so we make a dummy for PL2 before first move just to hold nodes in a tree --
  | state == Empty   = generateTreeStep (Node (Dummy PL2) []) [] 0 level
  | otherwise        = generateTreeStep state [] 0 $ level - 1

isDummy :: TMove -> Bool
isDummy move = case move of

  Dummy _ -> True
  otherwise -> False

generateTreeStep :: TGameState -> [TMove] -> Int -> Int -> TGameState
generateTreeStep state accMoves currLevel targetLevel

  | currLevel > targetLevel   = state
  | otherwise                 = case state of

      Empty             -> Empty
      Node move []      -> let newMoves = if isDummy move
                                          then
                                            accMoves
                                          else
                                            move:accMoves in case getPossibilities newMoves of

        []     -> Node move []
        poss   -> generateTreeStep (Node move (map (\x -> Node x []) poss)) newMoves currLevel targetLevel

      Node move nodes   -> Node move (foldr (\node acc -> (generateTreeStep node
                                                                            (if isDummy move
                                                                             then
                                                                               accMoves
                                                                             else
                                                                               (move:accMoves))
                                                                            (succ currLevel)
                                                                            targetLevel):acc) [] nodes)
isSubsetOf :: Eq a => [a] -> [a] -> Bool
isSubsetOf list1 list2 = length (list1 `intersect` list2) == length list1

winPositions :: [[(TPosX, TPosY)]]
winPositions = foldr (\curr acc -> [(x,y) | x <- [curr], y <- [(PY0)..]]:acc) [] [(PX0)..]
            ++ foldr (\curr acc -> [(x,y) | x <- [(PX0)..], y <- [curr]]:acc) [] [(PY0)..]
            ++ [zip [(PX0)..] [(PY0)..]] ++ [zip (reverse [(PX0)..]) [(PY0)..]]

isWin :: [TMove] -> TPlay -> Bool
isWin moves player

  | length moves < 5   = False
  | otherwise          = foldr (\x acc -> acc || x `isSubsetOf` playerMoves) False winPositions
  
  where playerMoves = map (\x -> let TMove pos player = x in pos)
                    . filter (\x -> getPlayer x == player)
                    $ moves

getGameResult :: [TMove] -> TPlay -> TGameResult
getGameResult moves player = if isWin moves player then Win else if isLoss moves player then Loss else Undefined

isLoss :: [TMove] -> TPlay -> Bool
isLoss moves player = isWin moves (switchPlayers player)

choose :: [Maybe (Int, TMove)] -> TPlay -> Maybe (Int, TMove)
choose [] _ = Nothing
choose list player = case filteredList of

    []      -> Nothing
    (x:_)   -> let Just (_, headMove) = x
                   comp = if getPlayer headMove == player then (>) else (<) in

                   foldl (\acc curr -> let Just (scoreCurr, move) = curr
                                           Just (scoreAcc, _) = acc in

                                           if scoreCurr `comp` scoreAcc
                                           then
                                             curr
                                           else
                                             acc) x filteredList 

  where filteredList = filter (/=Nothing) list

data TGameResult = Win | Undefined | Loss deriving (Show, Eq)

rate :: Int -> TGameResult -> Int
rate level result = case result of

  Win         -> 10
  Undefined   -> 0
  Loss        -> (-10)

cutTree :: TGameState -> TGameState
cutTree state = cutTreeStep state True

cutTreeStep :: TGameState -> Bool -> TGameState
cutTreeStep state cutting = case state of

  Empty              -> Empty
  Node move []       -> Node move []
  Node move [node]   -> if cutting then cutTreeStep node True else (Node move [cutTreeStep node False])
  Node move nodes    -> Node move (foldr (\node acc -> (cutTreeStep node False):acc) [] nodes)

minimax :: TGameState -> Int -> Maybe (Int, TMove)
minimax state difficulty = let decisionTree = cutTree $ generateDecisionTree state difficulty in
                               case decisionTree of
  Empty -> Nothing
  Node (Dummy player) _     -> minimaxStep decisionTree 0 (switchPlayers player) []
  Node (TMove _ player) _   -> minimaxStep decisionTree 0 player []

minimaxStep :: TGameState -> Int -> TPlay -> [TMove] -> Maybe (Int, TMove)
minimaxStep state currLevel player accMoves = case state of

  Node move [] -> Just (rate currLevel $ getGameResult (move:accMoves) player, move)
  Node move nodes -> (\res -> choose res player)
                   $ foldr (\node acc -> (minimaxStep node (succ currLevel) player (move:accMoves)):acc) [] nodes