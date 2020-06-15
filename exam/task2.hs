sort :: Ord a => [a] -> [a]
sort xs = inserts xs []

inserts :: Ord a => [a] -> [a] -> [a]
inserts [] r = r
inserts (x:xs) r = inserts xs (insert x r)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) =
  if x <= y
    then x : y : ys
    else y : insert x ys

main = do
  let input = [5, 11, 13, 16, 18, 1, 4, 8, 10, 20, 6, 3, 19, 7, 9, 2, 12]
  print $ sort input
  let input = [1, 8, 16, 7, 4, 17, 9, 12, 2, 3, 19, 11, 5, 20, 6, 14, 15]
  print $ sort input