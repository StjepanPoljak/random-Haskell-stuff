import Data.Foldable
import System.Process
import Control.Monad.State
import System.IO

gOrigin :: (Int, Int)
gOrigin = (10,1)

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
    waitForInput []
    
    showCursor
    clearScreen
    printStringOn (0,0) "Thank you for playing! Bye!\n" False

    return ()

drawAll :: IO ()
drawAll = do

    clearScreen

    height <- drawBoard gOrigin gWidth gHeight gPaddingX gPaddingY
    
    currPos <- printStringOn (screenPos gOrigin (PX2,PY1)
                                        gWidth gHeight
                                        gPaddingX gPaddingY) "#" False
    
    let prevY = (max (height + 1) (currPos + 1))
    
    putStrLn $ "\ESC[" ++ (show prevY) ++ ";0H"

    logDown ("drawing board: O" ++ (show gOrigin)
                                ++ " dim "
                                ++ (show $ length [(PX0)..])
                                ++ "×"
                                ++ (show $ length [(PY0)..])
                                ++ " each "
                                ++ (show gWidth)
                                ++ "×"
                                ++ (show gHeight)
                                ++ " pad "
                                ++ (show gPaddingX)
                                ++ "×"
                                ++ (show gPaddingY)
                                ++ " (press ESC to quit)") prevY

        -- **************************** KEYBOARD **************************** --

waitForInput :: [Char] -> IO ()
waitForInput prev = do
     x <- stdin `ifReadyDo` getChar
     takeAction x prev

takeAction :: Maybe Char -> [Char] -> IO ()
takeAction Nothing prev

        | prev == ['\ESC']           = return ()
        | prev == ['A','[','\ESC']   = do { logDown "Up!" 0; waitForInput [] }
        | prev == ['B','[','\ESC']   = do { logDown "Down!" 0; waitForInput [] }
        | prev == ['C','[','\ESC']   = do { logDown "Right!" 0; waitForInput [] }
        | prev == ['D','[','\ESC']   = do { logDown "Left!" 0; waitForInput [] }
        | prev == ['\n']             = do { logDown "Enter!" 0; waitForInput [] }
        | otherwise                  = waitForInput []

takeAction (Just char) prev = waitForInput (char:prev)

ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo hnd x = hReady hnd >>= f
   where f True = x >>= return . Just
         f _    = return Nothing

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

printBoxPart :: (Int, Int) -> Int -> Int -> Bool -> Bool -> Int -> Bool -> IO Int
printBoxPart (x, y) width height hasYPadding isTopRow currLevel selected =
    
    if currLevel == height + 1 then do return currLevel

    else do printStringOn (x, (y+currLevel))
                          (hBound:(putChars (width * 2 - 1) wBound)
                       ++ [hBound]) selected

            return =<< printBoxPart (x,y) width height
                                    hasYPadding isTopRow
                                    (currLevel + 1)
                                    selected

    where hBound = if currLevel == 0 && (hasYPadding || isTopRow) then ' ' else '|'
          wBound = if currLevel `elem` [0, height] then '_' else ' '

sub :: (Int, Int) -> (Int, Int) -> (Int, Int)
(x1, y1) `sub` (x2, y2) = (x1 - x2, y1 - y2)

drawBox :: (Int, Int) -> Int -> Int -> Bool -> Bool -> Bool -> IO Int
drawBox (x, y) width height hasYPadding isTopRow selected = do

                                                     newY <- printBoxPart (x, y)
                                                                          width
                                                                          height
                                                                          hasYPadding
                                                                          isTopRow 0
                                                                          selected
                                                     return (newY + y - 1)

printStringOn :: (Int, Int) -> String -> Bool -> IO Int
printStringOn (x, y) string bold = do putStrLn $ "\ESC[" ++ (show y)
                                                         ++ ";"
                                                         ++ (show x)
                                                         ++ "H"
                                                         ++ escBoldStart
                                                         ++ string
                                                         ++ escBoldEnd
                                      return y

                                   where escBoldStart = if bold then "\ESC[2m" else ""
                                         escBoldEnd = if bold then "\ESC[m" else ""

screenPos :: (Int, Int) -> (TPosX, TPosY) -> Int -> Int -> Int -> Int -> (Int, Int)
screenPos (x, y) (posX, posY) width height padX padY = (x + padX + 1
                                                          + tPosX * (width * 2 + padX),
                                                        y + padY
                                                          + tPosY * (height + padY))
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
    
  return =<< drawBox (screenPos (x, y) (curr)
                                sWidth sHeight
                                padX padY)
                      sWidth sHeight
                      (padY /= 0) (isTopRow curr)
                      False) 0 getAllPositions

isTopRow :: (TPosX, TPosY) -> Bool
isTopRow (_,PY0) = True
isTopRow _ = False

        -- ********************************** DATA ************************************ --

data TPosX = PX0 | PX1 | PX2 | PX3 deriving (Eq, Show, Enum)
data TPosY = PY0 | PY1 deriving (Eq, Show, Enum)

data TPlay = PL1 | PL2 deriving (Eq, Show)

data TMove = TMove (TPosX, TPosY) TPlay deriving (Eq, Show)

data TGameState = Empty | Node TMove ([TGameState]) deriving (Eq, Show)

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