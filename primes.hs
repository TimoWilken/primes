module Main where

import Data.Foldable
import System.Environment
import System.Exit
import System.IO
import Text.Printf

intSqrt :: Integral a => a -> a
intSqrt 0 = 0
intSqrt 1 = 1
intSqrt n = head . dropWhile (not . isRoot) $ iterate newtonStep start
  where start = lowerRoot * intSqrt (n `div` lowerN)
        (lowerRoot, lowerN) = last . takeWhile ((<=n) . snd)
          $ zip (1:squares) squares
        squares = iterate (^!2) 2
        newtonStep x = (x + n `div` x) `div` 2
        isRoot r = r^!2 <= n && (r+1)^!2 > n
        (^!) = (^) :: Integral a => a -> Int -> a

isPrime :: Integral a => a -> Bool
isPrime 1 = False
isPrime 2 = False
isPrime n = not $ any ((==0) . (n `mod`)) [2..intSqrt n]

primes :: [Integer]
primes = filter isPrime [1..]

-- Read a natural number; uses the preferred internal type.
readN :: String -> Integer
readN = read

checkPrime :: (Integral a, PrintfArg a) => a -> IO ()
checkPrime n | isPrime n = return ()
             | otherwise = do
                 progName <- getProgName
                 hPrintf stdout "%s: %d is not prime\n" progName n
                 exitWith $ ExitFailure 1

main :: IO ()
main = do
  args <- getArgs
  input <- getContents
  let numbers = lines input
  case args of
    [] -> traverse_ print primes
    a | "--help" `elem` a || "-h" `elem` a -> usage
    ["--check"] -> traverse_ (checkPrime . readN) numbers
    ["--stdin"] -> traverse_ putStrLn $ filter (isPrime . readN) numbers
    ["--stdin", numPrimes] ->
      traverse_ putStrLn . take (read numPrimes)
      $ filter (isPrime . readN) numbers
    [numPrimes] -> traverse_ print $ take (read numPrimes) primes
    _ -> usage

usage :: IO ()
usage = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++ " [--check | [--stdin] [NUM_PRIMES]]"
  putStrLn mempty
  putStrLn "  -h, --help  print this message and exit"
  putStrLn "  --check     check all numbers given on stdin for primeness"
  putStrLn "  --stdin     reads numbers from stdin; copies primes to stdout"
  putStrLn "  NUM_PRIMES  only print the first NUM_PRIMES primes;"
  putStrLn "              if not given, output is potentially unbounded"
  putStrLn mempty
  putStrLn "If --check is given, the exit code 0 indicates all numbers were"
  putStrLn "prime; 1 indicates that at least one non-prime was passed in."
  exitSuccess
