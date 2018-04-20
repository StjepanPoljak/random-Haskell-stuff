import Data.List

main :: IO()
main = do
     putStrLn(show(last $ largestPrime 600851475143))
     putStrLn $ show $ snd (findLargestPalindrome ([(x,y) | x<-[100..999], y<-[100..999]]))

largestPrime :: Integer -> [Integer]
largestPrime n = filter (\x -> null (largestPrime x)) $ filter (\x -> n `mod` x == 0 ) $ takeWhile (\x -> x*x < n) [2..]

fibEvenSum :: Integer -> Integer
fibEvenSum n = sum $ filter (\x -> x `mod` 2 == 0) $ fib n

fib :: Integer -> [Integer]
fib n = takeWhile (<n) $ fibs where fibs = (0 : 1 : zipWith (+) (fibs) (tail (fibs)))

mul :: Int -> Int -> [Int]
mul k n = takeWhile (<n) $ foldr (\x acc -> (k*x):acc) [] [1..]

mul3y5 :: Int -> Int
mul3y5 n = sum $ takeWhile (<n) $ union (mul 3 n) (mul 5 n)

getDigits :: Int -> [Int]
getDigits n
  | n < 0      = getDigits (-n)
  | n == 0     = []
  | otherwise  = getDigits (n `div` 10) ++ [n `mod` 10]

checkIfPalindrome :: [Int] -> Bool
checkIfPalindrome [] = True
checkIfPalindrome [x] = True
checkIfPalindrome (x:xs)
  | x /= y     = False
  | otherwise  = checkIfPalindrome (init xs)
  where y = last xs

findLargestPalindrome :: [(Int,Int)] -> ([((Int,Int),Int)],Maybe Int)
findLargestPalindrome list = foldl (\acc x -> let a = fst x
                                                  b = snd x
                                                  ab = a*b
                                                  maymaxel = snd acc in
                                                  if checkIfPalindrome (getDigits ab) then
                                                    (((a,b),ab):(fst acc),maymaxel)
                                                  else acc) ([],Nothing) (list)