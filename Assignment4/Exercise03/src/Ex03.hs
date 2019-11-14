{- ccoady Ciaran Coady -}
module Ex03 where
import Data.List ((\\))

-- Datatypes -------------------------------------------------------------------

-- do not change anything in this section !

-- Binary Tree
data BT a b
  = Leaf
  | Branch (BT a b) a b (BT a b)
  deriving (Eq, Show)

-- association list
type Assoc a b = [(a,b)]

-- lookup binary (search) tree
lkpBST :: Ord a1 => BT a1 a -> a1 -> Maybe a
lkpBST Leaf _  =  Nothing
lkpBST (Branch left k d right) k'
 | k < k'     =  lkpBST left k'
 | k > k'     =  lkpBST right k'
 | otherwise  =  Just d

-- Coding Part 1 (13 Marks)

-- insert into binary (search) tree
insBST :: Ord a => a -> b -> BT a b -> BT a b
insBST a b Leaf = Branch Leaf a b Leaf
insBST a b (Branch leftSubtree k d rightSubtree)
    | a < k     = Branch (insBST a b leftSubtree) k d rightSubtree
    | a > k     = Branch leftSubtree k d (insBST a b rightSubtree)
    | otherwise = Branch leftSubtree a b rightSubtree
                      
-- Coding Part 2 (6 Marks)

-- convert an association list to a binary search tree
assoc2bst :: Ord a => Assoc a b -> BT a b
assoc2bst [] = Leaf
assoc2bst [(x,y)] = insBST x y Leaf
assoc2bst ((x,y):xs) = insBST x y (assoc2bst xs)


-- Coding Part 3 (6 Marks)

-- convert a binary search tree into an (ordered) association list
bst2assoc :: Ord c =>  BT c e -> Assoc c e
bst2assoc Leaf = []
bst2assoc (Branch Leaf a b Leaf) = [(a,b)] 
bst2assoc (Branch leftSubtree a b rightSubtree) = bst2assoc leftSubtree ++ [(a,b)] ++ bst2assoc rightSubtree
