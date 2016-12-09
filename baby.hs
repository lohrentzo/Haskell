--doubleMe :: Num -> Num
doubleMe x = x+x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                        then x
                        else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1 

reverser :: [a] -> [a]
reverser [] = []
reverser (x:xs) = reverser xs ++ [x]

--reverse a = foldr (\b c -> c ++ [b] ) [] a -- -> not efficient!!
--reverse bs = foldr (\b g x -> g (b : x)) id bs []
--reverse = foldl' (flip (:)) []
--
--
factorial :: Integer -> Integer
factorial n = product [1..n]

facktorialPM :: (Integral a) => a -> a
facktorialPM 0 = 1
facktorialPM 1 = 1
facktorialPM n = n * facktorialPM (n-1)

facktorialIf :: (Integral a) => a -> a
facktorialIf n = if n == 0 then 1
                 else if n == 1 then 1
                 else n*facktorialIf(n-1)

facktorialGuards :: (Integral a) => a -> a
facktorialGuards n 
                 | n == 0 = 1
                 | n == 1 = 1
                 | otherwise = n*facktorialGuards(n-1)

