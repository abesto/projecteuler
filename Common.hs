module Common where

n `divides` k = (k `mod` n) == 0

divisibleBy = flip divides

isPalindrome n = (show n) == (reverse $ show n)

printSolution f n = do
  putStr $ show $ f n
  putStr " (N="
  putStr $ show n
  putStrLn ") "

runSolution :: (Show a, Show b) => String -> (a -> b) -> [a] -> IO ()
runSolution task solve inputs = do
  putStrLn task
  mapM_ (printSolution solve) inputs
