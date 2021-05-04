size = 50 -- increase me for more fun

main = putStrLn . unlines $ map (foldl (++) "") $ map (map show) expansion

expansion :: [[S]]
expansion = firstRow : (take size $ go firstRow)
    where firstRow :: [S]
          firstRow = (start ++ [D] ++ start)
          start :: [S]
          start = replicate size E
          go :: [S] -> [[S]]
          go xs = p : go p
              where p = process (head xs) xs

data S = L | R | D | E deriving Eq
instance Show S where
    show L = "["
    show R = "]"
    show D = "."
    show E = "="

process :: S -> [S] -> [S]
process last [current]
    | last == current = [E]
    | last /= current = [R]
process last (current : next : xs)
    | last == current && current == next = E : process current (next : xs)
    | last == current && current /= next = L : process current (next : xs)
    | last /= current && current == next = R : process current (next : xs)
    | last /= current && current /= next = D : process current (next : xs)
