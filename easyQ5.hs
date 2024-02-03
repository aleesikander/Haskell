data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

constructTree :: String -> Tree Char
constructTree str = fst $ construct str (Node ' ' Empty Empty) where
  construct [] current = (current, [])
  construct ('^':xs) (Node v l r) = 
    let (rightSubtree, rest) = construct xs (Node ' ' Empty Empty)
    in (Node v l rightSubtree, rest)
  construct (x:xs) Empty = 
    let (leftSubtree, rest) = construct xs (Node ' ' Empty Empty)
    in (Node x leftSubtree Empty, rest)
  construct (x:xs) (Node v l r) = 
    let (leftSubtree, rest) = construct xs l
    in if l == Empty 
       then (Node v (Node x Empty Empty) r, rest)
       else (Node v leftSubtree r, x:rest)


