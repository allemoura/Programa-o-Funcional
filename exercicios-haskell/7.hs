-- Achatar uma estrutura de lista aninhada.
data NestedList a = Elem a | List [NestedList a]

achatar :: NestedList a -> [a]
achatar (Elem x) = [x]
achatar (List []) = []
achatar (List (x:xs)) = achatar x ++ achatar (List xs)


main :: IO ()
main = do
    let achatado = achatar (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
    putStrLn $ show achatado