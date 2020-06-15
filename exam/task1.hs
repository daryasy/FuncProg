searchPair :: [Integer] -> Maybe Integer
searchPair (head:list) = if (searchNumber head list) then (Just head) else (searchPair list)
searchPair [] = Nothing

searchNumber :: Integer -> [Integer] -> Bool
searchNumber target (head:list) = if target == head then True else (searchNumber target list)
searchNumber target [] = False


main = print $ searchPair [1, 2, 3, 1]



