main :: IO()
main = do
  print (gameOfLife [(0,0),(0,1),(1,0),(1,1)])
  print (gameOfLife [(0,-1),(0,0),(0,1)])
  print (gameOfLife [(0,1),(1,2),(2,0),(2,1),(2,2)])
  
neighbour :: (Int, Int) -> (Int, Int) -> Bool
neighbour (x, y) (m, n) = abs (x - m) <= 1 && abs (y - n) <= 1
 
neighboursFromList :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
neighboursFromList lst x = [y| y <- lst, neighbour x y]
 
mapToNumberOfNeighbours :: [(Int, Int)] -> [Int]
mapToNumberOfNeighbours xs = map (\m -> m-1) (map length l)
                       where l = (map (neighboursFromList xs) xs)
 
remainAlive :: [(Int, Int)] -> [((Int, Int), Int)]
remainAlive brd = [(x, i)| (x, i) <- (zip brd (mapToNumberOfNeighbours brd)), (i ==2 || i ==3)]
 
remain :: [(Int, Int)] -> [(Int, Int)]
remain board = fst (unzip (remainAlive board))
                   
allNeighboursOf :: (Int, Int) -> [(Int, Int)]
allNeighboursOf (x, y) = [(x, y + 1), (x, y - 1),(x + 1, y), (x - 1, y), (x + 1, y + 1), (x + 1, y - 1), (x - 1, y - 1), (x - 1, y + 1)]
 
allouterNeighbours ::  [(Int, Int)] -> [(Int, Int)]
allouterNeighbours ls = [x| x <- concat (map allNeighboursOf ls), not (elem x ls)]

withoutRepeat :: Eq a => [a] -> [a]
withoutRepeat [] = []
withoutRepeat (x:xs)
      | elem x xs  = withoutRepeat xs
      | otherwise  = x:(withoutRepeat xs)

gameOfLife :: [(Int, Int)] -> [(Int, Int)]
gameOfLife board = remain board ++ [b| b <- withoutRepeat (allouterNeighbours board), length (neighboursFromList board b) == 3]