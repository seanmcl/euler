
length $ List.nub $ List.sort $ 
  (map (\(a, b) -> a^b) $ allPairs [2 .. 100] [2 .. 100])
