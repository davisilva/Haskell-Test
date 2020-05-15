
replace :: String -> String -> String -> String
replace [] _ _ = []
replace _ [] _ = []
replace _ _ [] = []
replace a b (x:xs) | checa       a (x:xs) = b ++ replace a b (takeonly a (x:xs))
                   | otherwise = x:replace a b xs

yeah :: Int -> [a] -> [Int]
yeah b x = [b.. (myLength x)-1]

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' a (x:xs) = getindexes(yeah a(x:xs)) (x:xs)

gotyou:: Int -> [a] -> [Int]
gotyou b x = [0..(b-1)]

getindexes :: [Int] -> [a] -> [a]
getindexes _ [] = []
getindexes [] _ = []
getindexes (a:as) bs = (bs !! a) : (getindexes as bs) 

take' :: Int -> [a] -> [a]
take' a (x:xs) = getindexes(gotyou a(x:xs)) (x:xs)

takeonly :: String -> String -> String
takeonly [] _ = []
takeonly _ [] = []
takeonly a (b:bs) = drop' (myLength a) (b:bs)

subseq :: String -> String -> Int
subseq [] _ = 0
subseq _ [] = 0
subseq (a:as) (b:bs) | a == b = 1 + subseq as bs
                     | otherwise = 0

checa :: String -> String -> Bool
checa [] _ = False
checa _ [] = False
checa xs ys | subseq (xs) (ys) == myLength (xs) = True
            | otherwise = False

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs
