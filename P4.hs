import Common

task = unlines ["A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99."
                ,"Find the largest palindrome made from the product of two 3-digit numbers."
                ]

solve :: Integer -> Integer
solve n = foldl max 0 [a*b | a <- divisorCandidates, b <- divisorCandidates, isPalindrome (a*b)]
          where maxDivisor = (10 ^ n) - 1
                divisorCandidates = [2..maxDivisor]

main = runSolution task solve [2, 3]
