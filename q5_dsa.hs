myLength :: Eq a => [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs


foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f a [] = a
foldr' f a (x:xs) = f x (foldr' f a xs) 

elem' :: Eq a => a -> [a] -> Bool
elem' x = foldr' (\a b -> a == x || b) False

inter :: Eq a => [a] -> [a] -> Int
inter _ [] = 0
inter [] _ = 0
inter (a:as) (x:xs) | a == x = 1 + inter as xs
                    | otherwise = 0 + inter as xs

test :: Eq a => [a] -> [a] -> Bool
test _ [] = False
test [] _ = True
test (x:xs) as | elem' x as = test xs as
               | otherwise = False 

testb :: Eq a => [a] -> [a] -> Bool
testb _ [] = True
testb [] _ = False
testb as (x:xs) | elem' x as = testb as xs
               | otherwise = False 

compareSets :: (Eq a) => [a] -> [a] -> String
compareSets [] _ = []
compareSets _ [] = []
compareSets a b | test a b && testb a b && length a == length b = "A = B"
                | test a b  = "A c B"
                | testb a b  = "B c A"
                | inter a b > 0 = "Intersection"
                | inter b a > 0= "Intersection"
                | inter a b == 0 = "Disjoint"
                | inter b a == 0 = "Disjoint"