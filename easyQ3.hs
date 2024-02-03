data List a = Nil | ListNode a (List a)
    deriving (Eq, Show)

palindromList :: Eq a => List a -> Bool
palindromList list = let regularList = toList list
                    in regularList == reverse regularList
  where
    toList :: List a -> [a]
    toList Nil = []
    toList (ListNode x xs) = x : toList xs

main :: IO ()
main = do
    print (palindromList (Nil :: List Int)) 
    print (palindromList (ListNode 1 (ListNode 2 (ListNode 3 (ListNode 2 (ListNode 1 Nil)))))) 
    print (palindromList (ListNode 1 (ListNode 2 (ListNode 3 (ListNode 3 (ListNode 1 Nil)))))) 
    print (palindromList (ListNode 1 (ListNode 2 (ListNode 3 (ListNode 2 (ListNode 2 Nil))))))
    print (palindromList (ListNode 1 (ListNode 2 (ListNode 3 (ListNode 2 (ListNode 1 (ListNode 1 Nil)))))))
    print (palindromList (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'b' (ListNode 'a' Nil)))))) 
    print (palindromList (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'c' (ListNode 'a' Nil))))))


    
