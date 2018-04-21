import Data.Foldable
import System.Process

guessWord :: String
guessWord = "interstellar"

placeholder::String
placeholder = "#"

centerString :: String -> Int -> String
centerString string cols = (putChars ((cols - (length string)) `div` 2) ' ')
                           ++ string

isNumeric :: Char -> Bool
isNumeric char = char `elem` "0123456789"

readFromTerminal :: String -> Int
readFromTerminal string = foldr (\x acc -> if isNumeric x
                                           then
                                             (acc + (read [x])) * 10
                                           else acc) 0 string

putChars :: Int -> Char -> String
putChars rep char
      | rep > 0         = char:putChars (rep - 1) char
      | rep == 0        = ""
      | otherwise       = putChars (-rep) char

main :: IO ()
main = do

     columnsRaw <- readProcess "tput" ["cols"] []
     let columns = readFromTerminal columnsRaw

     putStrLn $ "\n\ESC[2J\ESC[0;0H\n\ESC[1m" ++ (centerString "The Grand Haskell Guessing Game" columns) ++ "\ESC[m"
     putStrLn (centerString "by Stjepan Poljak" columns)

     putStrLn $ "\nType in a letter or take a guess!\nYou have "
              ++ (show guessLetters) ++ " attempts (type :q to quit).\n"


     printResult =<< foldlM (\acc x ->

              let step = (snd acc) <+ (snd x)
                  message = getStepString step (fst x) guessLetters in
               do putStrLn message
                  return $ acc +! (1, step))

              ((0, guessLetters), (generateGuessWord guessWord []))
       
       . zip [0..]
       . takeWhilePlus guessHasEmptyPlaces
       . map (\x -> if x == guessWord then fillOut x else processSingleGuess x)
       . takeWhile (/=":q")
       . take guessLetters
       . lines =<< getContents

     putStrLn "Thank you for playing!\n"

     where guessLetters = getNumOfLetters guessWord

data Letter = Letter Char | Empty deriving (Eq, Show)

data GuessWord = GuessWord [Letter] Bool deriving (Eq)

instance Show GuessWord where
  show (GuessWord [] _)       = "Empty."
  show (GuessWord ((Letter x):xs) _)
         | length xs > 0      = x:' ':(show (GuessWord xs True))
         | otherwise          = [x]
  show (GuessWord (Empty:xs) _)
         | length xs > 0      = placeholder ++ ' ':(show (GuessWord xs True))
         | otherwise          = placeholder

getNumOfLetters :: String -> Int
getNumOfLetters string = length result
  where result = foldl (\acc x -> if (x `elem` acc) then acc else x:acc) [] string

getAllPositions :: Char -> String -> [Int]
getAllPositions c str = foldr (\x acc -> if (c == (snd x))
                                         then
                                           (fst x):acc
                                         else acc)
                              [] (zip [0..] str)

generateGuessWord :: String -> [Int] -> GuessWord
generateGuessWord word list = GuessWord ( foldr (\x acc -> if (fst x `elem` list)
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
(<>) (Letter c) Empty = Letter c
(<>) Empty (Letter c) = Letter c
(<>) Empty Empty = Empty
(<>) (Letter c) (Letter d) = Letter c

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
                          "\nCool! " ++ residue
                        else
                          "\nDarn! " ++ residue
                        where residue = "Try a new letter " ++
                                        "or take a guess!\n\n" ++
                                        show (guess) ++ "\n"

-- current guess, current line and total lines --
getStepString :: GuessWord -> Int -> Int -> String
getStepString guess curr total = if curr < (total - 1)
                                    && (not $ guessHasEmptyPlaces guess)
                                 then
                                   "\nWow! Congrats!\n"
                                 else
                                   getNormalString guess

fillOut :: String -> GuessWord
fillOut string = GuessWord (map (\x -> (Letter x)) string) True

printNormalResult :: GuessWord -> IO ()
printNormalResult guess = if not $ guessHasEmptyPlaces guess
                          then do
                            putStrLn "You won!\n"
                          else do
                            putStrLn "You lost!\n"

printResult :: ((Int, Int), GuessWord) -> IO ()
printResult ((curr, total), guess) = if curr < (total - 1)
                                        && guessHasEmptyPlaces guess
                                     then
                                       putStrLn "\nYou quit! Why?\n"
                                     else printNormalResult guess

takeWhilePlus :: (a->Bool)->[a]->[a]
takeWhilePlus f list = (fst result) ++ customHead (snd result)
                       where result = span f list

customHead :: [a] -> [a]
customHead [] = []
customHead (x:xs) = [x]