data Tree a = Nil | TreeNode (Tree a) a (Tree a)
    deriving (Show, Eq)


symmetricTree :: Eq a => Tree a -> Bool
symmetricTree Nil = True
symmetricTree(TreeNode left _ right) = isMirror left right
  where
    isMirror :: Eq a => Tree a -> Tree a -> Bool
    isMirror Nil Nil = True
    isMirror (TreeNode l1 v1 r1) (TreeNode l2 v2 r2) = v1 == v2 && isMirror l1 r2 && isMirror r1 l2
    isMirror _ _ = False

main :: IO ()
main = do
        print(symmetricTree (Nil::Tree Int)) 
        print(symmetricTree (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 1 Nil))) 
        print(symmetricTree (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 2 Nil))) 
        print(symmetricTree (TreeNode (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil)) 4 (TreeNode (TreeNode Nil 3 Nil) 2 (TreeNode Nil 1 Nil)))) 
        print(symmetricTree (TreeNode (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil)) 4 (TreeNode (TreeNode Nil 3 Nil) 2 (TreeNode Nil 4 Nil))))
    
