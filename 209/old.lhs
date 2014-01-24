
-- > type Row = (Bool, Bool, Bool, Bool, Bool, Bool)


-- > rowToList :: Row -> [Bool]
-- > rowToList (a, b, c, d, e, f) = [a, b, c, d, e, f]

-- > listToRow :: [Bool] -> Row
-- > listToRow [a, b, c, d, e, f] = (a, b, c, d, e, f) 
-- > listToRow _ = error "Impossible" 

-- > rowToInt :: Row -> Int
-- > rowToInt = Lib.foldri (\i b n -> if b then 2^i + n else n) 0 . rowToList
-- >            
-- > intToRow :: Int -> Row
-- > intToRow n = listToRow (irow ++ replicate (6 - length irow) False)
-- >   where irow = row n
-- >         row 0 = []
-- >         row 1 = [True]
-- >         row n = if n `mod` 2 == 0 then False : rest else True : rest
-- >           where rest = row (n `div` 2)

-- If a row is assigned a 1, two rows are forced to be 0.

-- > forcedRow :: Row -> (Row, Row)
-- > forcedRow (a, b, c, d, e, f) = ( (b, c, d, e, f, a `xor` (b && c))
-- >                             , (not(a && b), a, b, c, d, e))

-- > forced :: Int -> (Int, Int)
-- > forced n = let (r1, r2) = forcedRow (intToRow n) in
-- >            (rowToInt r1, rowToInt r2)
