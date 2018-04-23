import Data.Foldable
import System.Process
import Control.Monad.State
import System.IO

gOrigin :: (Int, Int)
gOrigin = (10, 1)

gWidth :: Int
gWidth = 3

gHeight :: Int
gHeight = 4

gPaddingX :: Int
gPaddingX = 0

gPaddingY :: Int
gPaddingY = 0

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

        -- **************************** KEYBOARD **************************** --

waitForInput :: [Char] -> (TPosX, TPosY) -> IO ()
waitForInput prev pos = do
    
     x <- stdin `ifReadyDo` getChar
     takeAction x prev pos

move :: Direction -> (TPosX, TPosY) -> (Int, Int) -> Int -> Int -> Int -> Int -> IO (TPosX, TPosY)
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
        | prev == ['\n']             = do { logDown "Enter!" 0; waitForInput [] pos }
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

        -- ******************************** GUI ******************************** --

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

        -- ********************************** DATA ************************************ --

data TPosX = PX0 | PX1 | PX2 | PX3 deriving (Eq, Show, Enum)
data TPosY = PY0 | PY1 deriving (Eq, Show, Enum)

data TPlay = PL1 | PL2 deriving (Eq, Show)

data TMove = TMove (TPosX, TPosY) TPlay deriving (Eq, Show)

data TGameState = Empty | Node TMove ([TGameState]) deriving (Eq, Show)

data Direction = DLeft | DRight | DUp | DDown deriving (Eq, Show)

        -- ******************************** GAME LOGIC ******************************** --

addMove :: TMove -> State TGameState (Maybe TMove)
addMove newMove = do
          nodes <- get
          put (newMove +> nodes)
          return Nothing

(>+) :: TMove -> (Maybe [TMove]) -> (Maybe [TMove])
move >+ Nothing = Just [move]
move >+ (Just moves) = if move `elem` moves
                       then
                         Nothing
                       else
                         Just (move:moves)

(+>) :: TMove -> TGameState -> TGameState
move +> Empty = Node move []
move +> nodes = (Node move [nodes])

(>-) :: TMove -> TGameState -> TGameState
move >- Empty = move +> Empty
move >- (Node lastMove list) = (Node lastMove $ (Node move []):list)

accMoves :: TGameState -> (Maybe [TMove])
accMoves Empty = Nothing
accMoves (Node move []) = Just [move]
accMoves (Node move [node]) = move >+ (accMoves node)
accMoves (Node move list) = Nothing

traverseMoves :: TGameState -> (Maybe TMove)
traverseMoves Empty = Nothing
traverseMoves (Node move []) = Just move
traverseMoves (Node move [node]) = traverseMoves node
traverseMoves (Node move list) = Nothing

getLastMove :: State TGameState (Maybe TMove)
getLastMove = do
      lastState <- get
      return (traverseMoves lastState)

switchPlayers :: TPlay -> TPlay
switchPlayers PL1 = PL2
switchPlayers PL2 = PL1

--generatePossibleMoves :: TGameState -> TPlay -> Int -> (Maybe TGameState)
--generatePossibleMoves state player 0 = Just state
--generatePossibleMoves state player level = foldl (\acc x -> if not $ x `elem` disallowed then (generatePossibleMoves (x >- state) (switchPlayers player) (level - 1)) else acc) (Just Empty) getAllPositions
--                      where Just disallowed = (accMoves state)


--decide :: TGameState -> TPlayer -> Int -> TMove
--decide (Node move nodes) = 