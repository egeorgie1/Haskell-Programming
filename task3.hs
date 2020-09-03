main :: IO()
main = do
 print (isBalanced (Node 17 (Node 14  Empty Empty) (Node 20 (Node 20 Empty Empty) (Node 20 Empty Empty))) 2)

data Tree a = Empty | Node a (Tree a) (Tree a) 
                 deriving (Read, Show)

depth :: Tree a -> Int
depth  Empty         = 0
depth (Node _ t1 t2) = 1 + max (depth t1) (depth t2)

isBalanced :: Tree a -> Int -> Bool
isBalanced Empty _          = True
isBalanced (Node n t1 t2) k = abs (depth t1 - depth t2) <= k && isBalanced t1 k && isBalanced t2 k

