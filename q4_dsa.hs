first :: (a,b) -> a
first (a,b) = a

second :: (c,d) -> d
second (c,d) = d

filter' :: (a-> Bool) -> [a] -> [a]
filter' f x = [a | a<-x, f a]

tipoa :: (Ord a, Ord b) => [(a, b)] -> [(a, b)]
tipoa [] = []
tipoa (x:xs) = (tipoa men) ++ [x] ++ (tipoa mai)
        where
                mai = filter' (\c -> first c >= first x) xs
                men = filter' (\c -> first c < first x) xs

tipob :: (Ord a, Ord b) => [(a, b)] -> [(a, b)]
tipob [] = []
tipob (x:xs) = (tipob men) ++ [x] ++ (tipob mai)
        where
                mai = filter' (\c -> second c >= second x) xs
                men = filter' (\c -> second c < second x) xs

ordPairs :: (Ord a, Ord b) => [(a,b)] -> Int -> [(a,b)]
ordPairs [] _ = []
ordPairs xs a | a == 0 = tipoa xs
              | a == 1 = tipob xs

