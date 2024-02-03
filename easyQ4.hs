data Tree a = Nil | TreeNode (Tree a) a (Tree a) deriving (Show, Eq)

snakeTraversal :: Tree a -> [a]
snakeTraversal tree = concat $ zipWith ($) (cycle [id, reverse]) (levels tree)

levels :: Tree a -> [[a]]
levels Nil = []
levels tree = levelOrder [tree]
  where
    levelOrder [] = []
    levelOrder nodes = map nodeValue nodes : levelOrder (concatMap leftAndRight nodes)

    nodeValue (TreeNode x _ _) = x
    leftAndRight (TreeNode _ l r) = [l, r]
    leftAndRight Nil = []

main :: IO ()
main = do
        print(snakeTraversal (Nil:: Tree Int)) 
        print(snakeTraversal (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil)))
        print(snakeTraversal (TreeNode (TreeNode (TreeNode Nil 1 Nil) 3 (TreeNode Nil 6 Nil)) 4 (TreeNode (TreeNode Nil 5 Nil) 2 (TreeNode Nil 7 Nil)))) 
        print(snakeTraversal (TreeNode (TreeNode (TreeNode Nil 1 Nil) 3 (TreeNode Nil 6 Nil)) 4 (TreeNode (TreeNode Nil 5 Nil) 2 (TreeNode (TreeNode Nil 9 Nil) 7 Nil)))) 
    