map' :: (a -> b) -> [a] -> [b]

map' f [] = []
map' f (x:xs) = f x : map' f xs


data Tree a =
    Lf
  | Br a (Tree a) (Tree a) deriving Show


treeMember :: Eq a => a -> Tree a -> Bool

treeMember x Lf = False
treeMember x (Br y l r) =
  x == y || treeMember x l || treeMember x r


treeFlip :: Tree a -> Tree a

treeFlip Lf = Lf
treeFlip (Br x l r) = Br x (treeFlip r) (treeFlip l)


equalShape :: Tree a -> Tree b -> Bool

equalShape Lf Lf = True
equalShape (Br _ l r) (Br _ l2 r2) =
  equalShape l l2 && equalShape r r2
equalShape _ _ = False


treeInsert :: Ord a => Tree (a, b) -> a -> b -> Tree (a, b)

treeInsert Lf k v = Br (k, v) Lf Lf
treeInsert (Br (k', v') l r) k v =
  if k == k' then Br (k, v) l r
  else if k < k' then Br (k', v') (treeInsert l k v) r
  else Br (k', v') l (treeInsert r k v)


listOfTree :: Tree a -> [a]

listOfTree (Br x l r) = listOfTree l ++ [x] ++ listOfTree r
listOfTree Lf = []


treeOfList :: Ord a => [(a, b)] -> Tree (a, b)

treeOfList [] = Lf
treeOfList ((k, v):t) = treeInsert (treeOfList t) k v


treeUnion :: Ord a => Tree (a, b) -> Tree (a, b) -> Tree (a, b)

treeUnion t t' =
  treeOfList (listOfTree t ++ listOfTree t')
 

data MTree a = Branch a [MTree a] deriving Show


exampleMTree = Branch 1 [Branch 2 [], Branch 3 []]


sum' :: Num a => [a] -> a

sum' [] = 0
sum' (x:xs) = x + sum' xs


mTreeSize :: Num a => MTree b -> a

mTreeSize (Branch _ l) = 1 + sum' (map' mTreeSize l)


mTreeTotal :: Num a => MTree a -> a

mTreeTotal (Branch e l) = e + sum' (map' mTreeTotal l)


mTreeMap :: (b -> a) -> MTree b -> MTree a

mTreeMap f (Branch e l) = Branch (f e) (map' (mTreeMap f) l)

