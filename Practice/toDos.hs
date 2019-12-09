import Data.Char (toUpper) -- needed for Part 1
--toDos :: [Char] -> [Char]
--toDos [] = []
--toDos (x:xs) = toDosHelper 0 0 [] [] (x:xs)

toDosHelper :: Integer -> Integer -> Bool -> [Char] -> [Char]
toDosHelper _ _ _ [] = []
toDosHelper lenRoot lenExt afterDot (x:xs) | x == '.' = '.' : toDosHelper lenRoot lenExt True xs
                                                    | otherwise = 
                                                                    if(afterDot)
                                                                    then if (lenExt < 3) 
                                                                        then (toUpper x) : toDosHelper lenRoot (lenExt+1) afterDot xs
                                                                        else toDosHelper lenRoot lenExt afterDot []
                                                                    else if (lenRoot < 8)
                                                                        then (toUpper x) : toDosHelper (lenRoot+1) lenExt afterDot xs
                                                                        else toDosHelper lenRoot lenExt afterDot xs