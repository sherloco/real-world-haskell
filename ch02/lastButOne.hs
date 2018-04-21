-- file: ch02/lastButOne.hs
lastButOne xs = if length xs == 2
    then head xs
    else lastButOne (tail xs)

lastButOne2 (x:(y:[])) = x
lastButOne2 (x:xs) = lastButOne2 xs
