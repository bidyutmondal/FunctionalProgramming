
{-
//multiline comment
-}

--Single line comment

newlength  :: [Integer]->Integer
newlength [] = 0 
newlength (x:xs) = 1+newlength xs

newhead :: [Integer]->Integer
newhead [] = error "empty list"
newhead(x:xs) = x

newtail :: [Integer]->[Integer]
newtail [] = error "Empty list"
newtail (x:xs) = xs

newlast :: [Integer]->Integer
newlast []=error "Empty list"
newlast [x] = x
newlast (x:xs) = newlast xs

newreverse :: [Integer]->[Integer]
newreverse [] = []
newreverse (x:xs) = newreverse xs++[x]

newinit :: [Integer]->[Integer]
newinit [] = error "Empty list"
newinit [x]=[]
newinit (x:xs) = [x]++newinit xs

access :: [Integer]->Integer->Integer
access [] indx = error "Index out of bound"
access (x:xs) 0 = x
access (x:xs) idx = access xs (idx-1)

newsum :: [Integer] -> Integer
newsum[]=0
newsum(x:xs) = x + newsum(xs)


new_split_helper :: [Integer] -> [Integer] -> Integer -> ([Integer], [Integer])
new_split_helper l1 [] idx = error "Split - Index out of bounds !!"
new_split_helper l1 (x : xs) 0 = (l1 ++ [x], xs)
new_split_helper l1 (x : xs) idx = new_split_helper (l1 ++ [x]) xs (idx - 1)

new_split :: [Integer] -> Integer -> ([Integer], [Integer])
new_split l1 idx = new_split_helper [] l1 idx


newSplitHelper :: [Integer]->[Integer]->Integer->([Integer]->[Integer])
newSplitHelper a b 0 = a b
newSplitHelper a (x:xs) = newSplitHelper a++[x] xs i-1

newSplit :: [Integer]->Integer->([Integer]->[Integer])
newSplit x idx = newSplitHelper [] x idx 

power :: Integer->Integer->Integer
power x 0 = 1
power x n = x*(power x (n-1))


main = do
	let l1 = [6,8..20]
	print $ l1
	print $ l1++[49,50]
	print $ length l1
	print $ newlength l1
	print $ newhead l1
	print $ head l1
	print $ tail l1
	print $ newtail l1
	print $ newreverse l1
	print $ newlast l1
	print $ head (reverse l1)
	print $ init l1  --all but last
	print $ newinit l1
	print $ l1 !! 3
	print $ access l1 3
	print $ newsum l1
	print $ sum l1
	print $ splitAt 3 l1
	--print $ newSplit 3 l1
	print $ x+y
	print $ (+) x y
	print $ mod x y
	print $ x/y
	print $ x^y
	print $ div 3 0 --error
	print $ power (div 3 0) 0 --no error
	print $ reverse ['a'..'e']
	where x=15
		  y=10

