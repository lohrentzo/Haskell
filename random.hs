import System.Random

main = do
    gen <- getStdGen
    putStr $ take 20 (randomRs ('0','9') gen)
