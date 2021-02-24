newmap :: (a->b) ->[a]->[b]
newmap f [] = []
newmap f (x:xs) = (f x) : (newmap f xs)

is_even :: Int->Bool
is_even x = mode x 2 == 0

newfilter :: (a->Bool)->[a]->[a]
newfilter p [] = []
newfilter p (x:xs)
	|p x = x : (newfilter p xs)
	|otherwise = newfilter p xs

my_filter :: [Int] -> [Int]
my_filter [] = []
my_filter (x : xs)
    | mod x 2 == 0 = x : (my_filter xs)
    | otherwise = my_filter xs

insert :: Int->[Int]->[Int]
insert x [] = [x]
insert x (y:ys)
	|(x<=y) = x:y:ys
	|(x>y) = y : (insert x ys)

isort :: [Int]->[Int]
isort []=[]
isort (x:xs) = insert x (isort xs)

merge ([Int],[Int])->[Int]


main = do
	let l1 = [6,8..20]
	let l2 = []
	let l3 = [3,2,10,7,41,8,13]
	print $ l1
	print $ map(+3) l1
	print $ map(+3) []
	print $ newmap(+3) l1
	print $ newmap(*2) l1
	print $ filter is_even l3
	print $ newfilter is_even l3
	print $ my_filter l3
	print $ foldr (+) 0 l3
	print $ take 3 l3
	print $ drop 3 l3
	