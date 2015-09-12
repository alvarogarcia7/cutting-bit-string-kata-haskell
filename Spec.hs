main =putStrLn "hello"

toBinary :: Integer -> [Integer]
toBinary 0 = [0]
toBinary n = reverse $ toBinary' [] maxExponent n
    where maxExponent = floor (logBase 2 n)

toBinary'  :: (Ord a, Num a1, Num a, Integral b, Eq a1) => [a1] -> b -> a -> [a1]
toBinary' xs (-1) _ = xs
toBinary' xs exponent n = toBinary' (current : xs) (exponent - 1) (rest)
    where currentNumber = 2^exponent
    	  current = if n >= currentNumber then 1 else 0
    	  rest = if current == 1 then n - currentNumber else n