contrario :: String -> String
contrario [] = []
contrario (x:xs) = contrario xs ++ [x]

inverse :: [String] -> String
inverse [] = []
inverse xs = (juntalista (myMap aux xs))

juntalista :: [[a]] -> [a]
juntalista [] = []
juntalista (x:xs) = x ++ juntalista xs

aux :: String -> String
aux [] = []
aux xs = troca (contrario xs) 

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f a [] = a
foldr' f a (x:xs) = f x (foldr' f a xs) 

myMap :: (a->b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs 

islower :: Char -> Bool
islower x = elem' x ['a'..'z']

elem' :: Eq a => a -> [a] -> Bool
elem' x = foldr' (\a b -> a == x || b) False

isupper :: Char -> Bool
isupper x = elem' x ['A'..'Z']

maiusculo :: Char -> Char
maiusculo a | islower a = (['a'..'z'] ++ ['A'..'Z']) !! ((finda a (['a'..'z'] ++ ['A'..'Z']))+26)
            | otherwise = a 

minusculo :: Char -> Char
minusculo a | isupper a = (['a'..'z'] ++ ['A'..'Z']) !! ((finda a (['a'..'z'] ++ ['A'..'Z']))-26) 
            | otherwise = a

finda :: Eq a => a -> [a] -> Int
finda a [] = 0
finda a (x:xs) | a == x = 0
               | otherwise = 1+ finda a xs

troca :: String -> String
troca [] = []
troca (x:xs) | islower x = maiusculo x : troca xs
             | otherwise = minusculo x : troca xs 