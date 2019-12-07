noVowels :: [Char] -> [Char]
noVowels "" = ""
noVowels (x:xs)
        | x `elem` "aeiouAEIOU" = noVowels xs
        | otherwise = x : noVowels xs

foldl12 :: (a -> a -> a) -> [a] -> a
foldl12 _ [] = error "cannot apply fold to an empty list"
foldl12 op (x:xs) = error "not implemented yet"

foldlhelper :: (a -> a -> a) -> a -> [a] -> a
foldlhelper _ acc [] = acc
foldlhelper op acc (x:xs) = foldlhelper op newAcc xs
                            where newAcc = op acc x 

foldrhelper :: (a -> a -> a) -> [a] -> a
foldrhelper _ [] = error "connot apply fold to an empty list"
foldrhelper _ [x] = x
foldrhelper op (x:xs) = op x (foldrhelper op xs)

mySpan :: (a -> Bool) -> [a] -> ([a],[a])
mySpan _ [] = ([], [])
mySpan p (x:xs) | p x = let (before, after) = mySpan p xs
                        in (x:before, after)
                | otherwise = ([], (x:xs))

(!!!) :: [a] -> Int -> a
(!!!) (x:xs) n | n == 0 = x
              | otherwise = (!!!) xs (n-1)


hof :: a -> (a -> a -> a) -> [a] -> a
hof base function [] = base
hof base function (x:xs) = function x (hof base function xs)

myMin :: Ord a => [a] -> a
myMin [x] = x
myMin (x:xs)    | x <= rest = x
                | otherwise = rest
                where rest = myMin xs