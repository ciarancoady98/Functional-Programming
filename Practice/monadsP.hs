data BinTree = BNil
            | BOne Int String
            | BTwo BinTree Int String BinTree

myLookup :: Monad m => BinTree -> Int -> m String

myLookup (BOne i s) x
            | x == i = return s
            | otherwise = fail ("The Key " ++ (show x) ++ " was not found in the tree")

myLookup (BTwo left i s right) x
            | x < i = myLookup left x
            | x > i = myLookup right x
            | x == i = return s
            | otherwise = fail ("The Key " ++ (show x) ++ " was not found in the tree")

myLookup (BNil) x = fail ("The Key " ++ (show x) ++ " cannot be found in an empty tree")
