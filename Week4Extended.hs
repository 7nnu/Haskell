split2 :: [(a,b)] -> ([a],[b])
split2 xs = ([x | (x,_) <- xs], [y | (_,y) <- xs])