type Graph = [(Int,[Int])]

EmptyGraph :: Grafo 
EmptyGraph = []

listGraph :: [a] -> a
listGraph (x:xs) = x

insertNodeOnGraph :: Grafo -> Int -> Grafo 
insertNodeOnGraph xs x = xs ++ [(x,[])]

GraphNodes :: [Int] -> Grafo 
GraphNodes [] = []
GraphNodes (x:xs) = [(x,[])]++GraphNodes xs

insertArcInGraph :: Grafo -> Int -> Int ->  Grafo
insertArcInGraph [] a b = [(a,[b])]
insertArcInGraph ((x,gs):xs) a b 
    | a == x = [(x,insertinList gs b)] ++ xs
    | a /= x = [(x,gs)] ++ insertArcInGraph xs a b

insertinList :: [Int] -> Int -> [Int]
insertinList [] b = [b]
insertinList (x:xs) b
    | x == b = [x] ++ xs
    | x /= b = [x] ++ insertinList xs b

