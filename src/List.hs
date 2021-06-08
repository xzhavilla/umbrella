module List where

zipAdjacent (x : x' : xs) = (x, x') : zipAdjacent xs
zipAdjacent _ = []

unzipAdjacent [] = []
unzipAdjacent ((x, x') : xs) = x : x' : unzipAdjacent xs
