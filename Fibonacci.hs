module Fibonacci (fibonacci) where

data FibState = FibState Integer Integer

getFibStateNumber :: FibState -> Integer
getFibStateNumber (FibState a b) = b

nextFib :: FibState -> FibState
nextFib (FibState a b) = FibState b (a+b)

runFib :: FibState -> [Integer]
runFib s = (getFibStateNumber s):(runFib $ nextFib s)

fibonacci = runFib $ FibState 0 1

main = putStrLn $ show $ take 5 $ fibonacci
