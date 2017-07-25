-- 1
myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast (x:[]) = Just x
myLast (x:xs) = myLast xs

-- 2
mySecondToLast :: [a] -> Maybe a
mySecondToLast [] = Nothing
mySecondToLast (x:[]) = Nothing
mySecondToLast (x:y:[]) = Just x
mySecondToLast (x:xs) = mySecondToLast xs

-- 3
elementAt :: (Integral b) => [a] -> b -> Maybe a
elementAt [] _ = Nothing
elementAt (x:xs) 1 = Just x
elementAt (x:xs) i
  | i < 1 = Nothing
  | otherwise = elementAt xs (i - 1)

-- 4
myLength :: (Integral b) => [a] -> b
myLength [] = 0
myLength (x:xs) = 1 + (myLength xs)


-- 5
myReverse :: [a] -> [a]
myReverse list = recurseReverse [] list
  where recurseReverse result (x:xs) = recurseReverse (x:result) xs
        recurseReverse result [] = result

-- 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list = (myReverse list) == list

-- 7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = (flatten x) ++ (flatten (List xs))

-- 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:[]) = [x]
compress (x:y:xs)
  | x == y = compress (y:xs)
  | otherwise = x:(compress (y:xs))

-- 9

packRecurse :: (Eq a) => [a] -> [[a]] -> [a] -> [[a]]
packRecurse collector@(y:_) result rest@(x:xs)
  | y == x = packRecurse (x:collector) result xs
  | otherwise = packRecurse [] (collector:result) rest
packRecurse [] result (x:xs) = packRecurse [x] result xs
packRecurse [] result [] = result
packRecurse collector result [] = collector:result

pack :: (Eq a) => [a] -> [[a]]
pack list = myReverse $ packRecurse [] [] list

-- 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode list = map ((,) <$> length <*> head) $ pack list

-- 11

data RunLength a = Multiple Int a | Single a deriving (Show, Eq)

encode' :: (Eq a) => [a] -> [RunLength a]
encode' = translate . encode
  where translate [] = []
        translate (x:xs)
          | fst x == 1 = (Single (snd x)):(translate xs)
          | otherwise = (Multiple (fst x) (snd x)):(translate xs)

-- 12
decode' :: [RunLength a] -> [a]
decode' [] = []
decode' ((Single x):xs) = x:(decode' xs)
decode' ((Multiple k x):xs) = (replicate k x) ++ (decode' xs)

-- 13
incrementLength :: RunLength a -> RunLength a
incrementLength (Single x) = Multiple 2 x
incrementLength (Multiple i x) = Multiple (i + 1) x

value :: RunLength a -> a
value (Single x) = x
value (Multiple _ x) = x

encodeDirect :: (Eq a) => [a] -> [RunLength a]
encodeDirect [] = []
encodeDirect (x:xs) = myReverse (encodeRecurse (Single x) [] xs)
  where encodeRecurse c result (x:xs)
          | (value c) == x = encodeRecurse (incrementLength c) result xs
          | otherwise = encodeRecurse (Single x) (c:result) xs
        encodeRecurse c result [] = c:result

-- 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:(dupli xs)

-- 15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) i = (replicate i x) ++ (repli xs i)

-- 16
dropEvery :: (Integral b) => [a] -> b -> Maybe [a]
dropEvery [] _ = Just []
dropEvery xs i
  | i <= 0 = Nothing
  | otherwise = Just (recurseDrop xs 0 i)
  where recurseDrop [] _ _ = []
        recurseDrop (x:xs) k i
          | k == i = recurseDrop xs 0 i
          | otherwise = x:(recurseDrop xs (k + 1) i)

-- 17
split :: [a] -> Int -> Maybe ([a], [a])
split xs k
  | (length xs) < k = Nothing
  | otherwise = Just (splitRecurse [] k xs)
  where splitRecurse ys i xs
          | (length ys) == i = ((myReverse ys), xs)
          | otherwise = splitRecurse ((head xs):ys) i (tail xs)

-- 18
slice :: [a] -> Int -> Int -> [a]
slice xs a b = myReverse (sliceRecurse xs [] a b 1)
  where sliceRecurse [] _ _ _ _ = []
        sliceRecurse (x:xs) result a b i
          | i < a = sliceRecurse xs [] a b (i + 1)
          | i <= b = sliceRecurse xs (x:result) a b (i + 1)
          | i > b = result

-- 19
rotate :: [a] -> Int -> [a]
rotate xs 0 = xs
rotate [] _ = []
rotate (x:[]) _ = [x]
rotate (x:xs) r
  | r > 0 = rotate (xs ++ [x]) (r - 1)
  | r < 0 = rotate ((last xs):x:(init xs)) (r + 1)

-- 20
removeAt :: Int -> [a] -> (a, [a])
removeAt i xs = removeRecurse xs i 1 []
  where removeRecurse (x:xs) i k result
          | i == k = (x, (reverse result) ++ xs)
          | otherwise = removeRecurse xs i (k + 1) (x:result)
