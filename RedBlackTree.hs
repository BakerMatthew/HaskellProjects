-- Data Types
data Color = Red | Black deriving Show
data Tree x = Empty | Node Color (Tree x) x (Tree x) deriving Show

-- Insert Node
insert Empty v = Node Red Empty v Empty
insert (Node c l v r) x
    | x == v = Node c l v r
    | x < v = balance (Node c (insert l x) v r)
    | x > v = balance (Node c l v (insert r x))

-- Balance Tree
balance (Node Black (Node Red a x (Node Red b y c)) z d) = Node Red (Node Black a x b) y (Node Black c z d)
balance (Node Black (Node Red (Node Red a x b) y c) z d) = Node Red (Node Black a x b) y (Node Black c z d)
balance (Node Black a x (Node Red (Node Red b y c) z d)) = Node Red (Node Black a x b) y (Node Black c z d)
balance (Node Black a x (Node Red b y (Node Red c z d))) = Node Red (Node Black a x b) y (Node Black c z d)
balance t = t

-- Change Color
makeBlack (Node Red l v r) = Node Black l v r
makeBlack t = t

-- Insert [t] into Tree
treeInsert v t = makeBlack (insert t v)

-- Get Height
height Empty = 0
height (Node _ l v r) = 1 + max (height l) (height r)

-- Print Tree
display Empty = "Tree is empty."
display t = unlines (display_helper t)

display_helper (Empty) = []
display_helper (Node _ l v r) = (show v) : (display_subtree l r)
    where
         display_subtree l r = ((shift "+- " "|  ") (display_helper r)) ++ ((shift "~- " "   ") (display_helper l))

shift head tail = zipWith (++) (head : repeat tail)

display_subtree l r = ((shift "+- " "|  ") (display_helper l)) ++ ((shift "~- " "   ") (display_helper r))
