
merge :: [a]->[a]->[a]
merge [] y = y
merge x [] = x
merge (x:xs) (y:ys) =
     | x<y =  x:merge xs (y:ys)
     |  otherwise = y:merge (x:xs) ys

msort :: [a]->[a]
msort  l = merge take div (length l ) 2 l
drop div (length l ) 2
 
is_prime :: Int -> Bool
is_prime 1 = False
is_prime 2 = True
is_prime n
    | (length [x | x <- [2..(n - 1)], mod x n == 0] == 0) = True
    | otherwise = False

main = do
    let l1=[3,5..17]
    let l2=[2,4..16]
    print $ merge l1 l2
    print $ is_prime 37
    print $ [x | x<-[1..100], is_prime x ==True]
    print $ sieve [2..n]
    where
    	n=100
    	sieve (x:xs) = x (seive [y | y <- xs, mod y x >0])