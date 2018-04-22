import Data.Foldable
import System.Process

placeholder::String
placeholder = "#"

main :: IO ()
main = do

     printHeader

     columns <- getColumns

     putStrLn ""
     putStrLn $ centerString "Type in a letter or take a guess!" columns
     putStrLn $ centerString ("You have "
             ++ (show guessLetters)
             ++ " attempts.") columns
     putStrLn $ "\ESC[2m"
             ++ (centerString "(type :q to quit)" columns)
     putStrLn "\ESC[m"

     printResult =<< foldlM (\acc x ->
              
              let step = (snd acc) <+ (snd x)
                  message = getStepString step (fst x) guessLetters in
               do printHeader
                  putStrLn ""
                  putStrLn $ centerString message columns
                  putStrLn $ centerString ("You have "
                          ++ (show $ guessLetters - (fst $ fst acc) - 1)
                          ++ " more attempts.") columns
                  putStrLn $ "\ESC[2m"
                          ++ (centerString "(type :q to quit)" columns)
                  putStrLn "\ESC[m"
                  putStrLn $ centerString (show step) columns
                  return $ acc +! (1, step))

              ((0, guessLetters), (generateGuessWord guessWord []))
       
       . zip [0..]
       . takeWhilePlus guessHasEmptyPlaces
       . map (\x -> if x == guessWord then fillOut x else processSingleGuess x)
       . takeWhile (/=":q")
       . take guessLetters
       . lines =<< getContents

     putStrLn $ "\ESC[3m"
             ++ (centerString "Thank you for playing!" columns)
             ++ "\ESC[m\n"

     where guessLetters = getNumOfLetters guessWord

data Letter = Letter Char | Empty deriving (Eq, Show)

data GuessWord = GuessWord [Letter] Bool deriving (Eq)

instance Show GuessWord where
  show (GuessWord [] _)       = ""
  show (GuessWord ((Letter x):xs) _)
         | length xs > 0      = x:' ':show (GuessWord xs True)
         | otherwise          = [x]
  show (GuessWord (Empty:xs) _)
         | length xs > 0      = placeholder ++ ' ':show (GuessWord xs True)
         | otherwise          = placeholder

getNumOfLetters :: String -> Int
getNumOfLetters string = length result
  where result = foldl (\acc x -> if x `elem` acc then acc else x:acc)
                       [] string

getAllPositions :: Char -> String -> [Int]
getAllPositions c str = foldr (\x acc -> if (c == (snd x))
                                         then
                                           (fst x):acc
                                         else acc)
                              [] (zip [0..] str)

generateGuessWord :: String -> [Int] -> GuessWord
generateGuessWord word list = GuessWord ( foldr (\x acc -> if fst x `elem` list
                                                           then
                                                             Letter (snd x):acc
                                                           else
                                                             Empty:acc)
                                                [] (zip [0..] word) )
                                        ( length list /= 0 )

processGuess :: [String] -> [GuessWord]
processGuess list = map (\x -> processSingleGuess x) list

processSingleGuess :: String -> GuessWord
processSingleGuess [] = GuessWord [] False
processSingleGuess (x:xs) = let guessChar = x
                                fills = getAllPositions guessChar guessWord in
                                generateGuessWord guessWord fills

(<+) :: GuessWord -> GuessWord -> GuessWord
(<+) (GuessWord word1 guess1)
     (GuessWord word2 guess2) = GuessWord ( map (\x -> (fst x) <> (snd x))
                                              (zip word1 word2) ) guess2

(<>) :: Letter -> Letter -> Letter
(Letter c) <> Empty = Letter c
Empty <> (Letter c) = Letter c
Empty <> Empty = Empty
(Letter c) <> (Letter d) = Letter c

(+!) :: ((Int,Int), a) -> (Int, a) -> ((Int,Int), a)
((first, second), a) +! (number, b) = ((first + number, second), b)

guessHasEmptyPlaces :: GuessWord -> Bool
guessHasEmptyPlaces (GuessWord letters _) = foldr (\x acc -> if (acc == True)
                                                             then
                                                               True
                                                             else
                                                               isEmptyLetter x)
                                                  False letters
isEmptyLetter :: Letter -> Bool
isEmptyLetter Empty = True
isEmptyLetter _ = False

isMissGuess :: GuessWord -> Bool
isMissGuess (GuessWord _ truth) = not truth

getNormalString :: GuessWord -> String
getNormalString guess = if not $ isMissGuess guess
                        then
                          "Cool! " ++ residue
                        else
                          "Darn! " ++ residue
                        where residue = "Try a new letter " ++
                                        "or take a guess!"

-- current guess, current line and total lines --
getStepString :: GuessWord -> Int -> Int -> String
getStepString guess curr total = if curr < (total - 1)
                                    && (not $ guessHasEmptyPlaces guess)
                                 then
                                   "Wow! Congrats!"
                                 else
                                   getNormalString guess

fillOut :: String -> GuessWord
fillOut string = GuessWord (map (\x -> (Letter x)) string) True

printNormalResult :: GuessWord -> IO ()
printNormalResult guess = if not $ guessHasEmptyPlaces guess
                          then do
                            columns <- getColumns
                            putStrLn $ "\n\ESC[1m"
                                    ++ (centerString "You won!" columns)
                                    ++ "\ESC[m"
                          else do
                            columns <- getColumns
                            putStrLn $ "\n"
                                    ++ (centerString "You lost!" columns)
                                    ++ ""

printResult :: ((Int, Int), GuessWord) -> IO ()
printResult ((curr, total), guess) = do

                              columns <- getColumns

                              if curr < (total - 1)
                                 && guessHasEmptyPlaces guess
                              then do
                                putStrLn $ "\ESC[5m"
                                        ++ (centerString "You quit! Why?" columns)
                                        ++ "\ESC[m"
                              else
                                printNormalResult guess

takeWhilePlus :: (a->Bool)->[a]->[a]
takeWhilePlus f list = (fst result) ++ customHead (snd result)
                       where result = span f list

customHead :: [a] -> [a]
customHead [] = []
customHead (x:xs) = [x]

centerString :: String -> Int -> String
centerString string cols = (putChars ((cols - (length string)) `div` 2) ' ')
                           ++ string

isNumeric :: Char -> Bool
isNumeric char = char `elem` "0123456789"

readFromTerminal :: String -> Int
readFromTerminal string = foldl (\acc x -> if isNumeric x
                                           then
                                             (acc * 10 + (read [x]))
                                           else acc) 0 string
guessWord :: String
guessWord = "haskell"

putChars :: Int -> Char -> String
putChars rep char
      | rep > 0         = char:putChars (rep - 1) char
      | otherwise       = ""

getColumns :: IO Int
getColumns = do
     columnsRaw <- (readProcess "tput" ["cols"] [])
     let columns = readFromTerminal columnsRaw
     return columns

printHeader :: IO ()
printHeader = do

     columns <- getColumns

     putStrLn $ "\n\ESC[2J\ESC[0;0H\n\ESC[1m"
           ++ (centerString "The Grand Haskell Guessing Game" columns)
           ++ "\ESC[m"
     putStrLn (centerString "by Stjepan Poljak" columns)
     putStrLn (putChars columns '_')