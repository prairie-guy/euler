module Main where

countUntil' p xs = cu p xs 0
                   where cu p (x:xs) n = if (p x) then n else cu p xs $! n+1

afn n = 1 + countUntil' (==1) (iterate (\k -> mod (k*10) (9*n)) 10)

ans129 q = head $ dropWhile ((<=q) . afn) [n|n<-[q..], mod n 2/=0, mod n 5/=0]

main = print $ ans129 (10^6)

                   
