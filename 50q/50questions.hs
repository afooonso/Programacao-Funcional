
import Data.List
import Data.Time.Format.ISO8601 (yearFormat)

--11
regroup :: Eq a => [a] -> [[a]]
regroup [] = []
regroup [m]= [[m]]
regroup (x:xs)
              |x== head (xs)= (x:head (regroup xs)) :tail (regroup xs)
              |otherwise= [x] :(regroup xs)


--12
concatenate :: [[a]] -> [a]
concatenate [[]]= []
concatenate [[m]]= [m]
concatenate (x:y) = x ++concat y
--13
inits' :: [a] -> [[a]]
inits' []= [[]]
inits' (l)= (inits'(init l)) ++ [l]
--14 
tails' :: [a]-> [[a]]
tails' []= [[]]
tails' l = [l] ++ tails' (tail l)
--15
heads' :: [[a]] -> [a]
heads' []= []
heads' ([]:l)= heads' l
heads' (x:l)= head x : heads' l
--16
total :: [[a]] -> Int
total [] = 0
total (x:l) = length x + total l
--17
fun :: [(a,b,c)]-> [(a,c)]
fun []= []
fun ((a,b,c):d)= (a,c): fun d
--18
cola:: [(String,b,c)] -> String
cola []= ""
cola ((a,b,c):d)= a++(cola d)
--19
idade :: Int -> Int -> [(String,Int)] -> [String]
idade _ _ []= []
idade a b ((c,d):f)
                     |abs (a-d)>=b = c:(idade a b f)
                     | otherwise= idade a b f
--20
powerEnumFrom:: Int-> Int-> [Int]
powerEnumFrom n 0=[]
powerEnumFrom n m
                 | m>1 = powerEnumFrom n (m-1) ++ [n^(m-1)]
                 |otherwise= [1]
--21
isPrime :: Int->Bool
isPrime 2 =True
isPrime 1 = False
isPrime n= isprime1 n 2

isprime1 :: Int->Int->Bool
isprime1 n 2 
               |(length[x| x<- [2 .. n-1], mod n x==0 ])>=1 = False
               | otherwise = True             

primeCheck :: Int -> Int -> Bool
primeCheck n m
    | m * m > n = True
    | mod n m == 0 = False
    | otherwise = primeCheck n (m + 1)

--22
isPrefixOff ::Eq a => [a] -> [a]-> Bool
isPrefixOff [] _ = True
isPrefixOff (a:b) (c:d)
                      | a==c = isPrefixOff b d
                      | otherwise =False
--23
isSuffixOff :: Eq a => [a]-> [a] -> Bool
isSuffixOff [] _ =True
isSuffixOff _ [] = False
isSuffixOff (a:b) (c:d)
                       |a==head d = isSuffixOff b (d)
                       |otherwise=False

--24
isSubsequenceOff :: Eq a =>[a] -> [a] -> Bool
isSubsequenceOff  [] [] = True 
isSubsequenceOff  [] _ = True
isSubsequenceOff _ [] = False
isSubsequenceOff  (x:xs) (y:ys)
                              | x==y = isSubsequenceOff xs ys
                              | otherwise = isSubsequenceOf (x:xs) ys

--25

elemIndicess :: Eq a => a ->[a] -> [Int]
elemIndicess _ [] = []
elemIndicess n y = helpelemindices 0 n y 

helpelemindices:: Eq a=> Int->a->[a]->[Int]
helpelemindices i n (x:xs)
                          | n ==x  = i : helpelemindices (i+1) n (xs)
                          | otherwise = helpelemindices (i+1) n (xs)

--26

nubb :: Eq a => [a]-> [a]

nubb [] = []
nubb (x:xs)= inserir (x) (nubb xs)

inserir :: Eq a=> a -> [a]-> [a]
inserir a [] = [a]
inserir a (x:xs) 
                | a == x = x:xs 
                |otherwise = inserir a xs ++ [x] 

nub2 :: Eq a => [a]-> [a] 
nub2 []= []
nub2 (k)
        | last k `elem` init k = nub2 (init k)
        | otherwise = init k ++ [last k ]
                  

--27
deletee :: Eq a => a -> [a]->[a]
deletee _ [] = []
deletee n (x:xs)
               | n == x = xs
               |otherwise = x : (deletee n xs)

--28 
barra :: Eq a => [a] -> [a]->[a]
barra [] [] = []
barra [] _ = []
barra l [] = l
barra (x:xs) (y)
                   | x `elem` y = barra (xs) (tail y) 
                   | otherwise = x : barra xs y

--29
unionn :: Eq a => [a]-> [a] -> [a]
unionn [] _ = []
unionn l [] = l 
unionn (x) (y:ys) 
               | y `elem` x  = unionn (x) (ys) 
               | otherwise = unionn (x) (ys) ++ [y]

--30
inter :: Eq a => [a]-> [a]-> [a]
inter [] _ = []
inter l [] = l 
inter (x:xs) y 
                   | x `elem` y = x:inter (xs) (y)
                   | otherwise =  inter xs (y)


--31 
myinsert :: Ord a => a -> [a]-> [a]
myinsert a [] = [a]
myinsert a (x:xs)
               | a<=x = a:x:xs 
               |otherwise = x:myinsert a  xs 


--32 
myunwords :: [String]-> String 
myunwords [] = ""
myunwords (x:xs) = x ++ (if null xs then "" else " ") ++ myunwords xs

--33

pMaior :: Ord a=> [a]->Int 
pMaior [_]= 0 
pMaior (x:xs) 
              | x> head xs = pMaior (x:tail xs)
              | otherwise = 1 + pMaior xs



--35

mylookup :: Eq a=> a -> [(a,b)]->Maybe b 
mylookup _ [] = Nothing
mylookup a (x:xs)
                | a == x1 = Just (x2 )
                |otherwise =  mylookup  a xs 
                where (x1,x2)= (fst x, snd x)
            

--36

preCrescente :: Ord a => [a] -> [a]
preCrescente [] = [] 
preCrescente [a] = [a]
preCrescente (x:y:xs)
                   |y>=x =  x: preCrescente (y:xs)
                   | otherwise = [x] 


preCrescente2 :: Ord a => [a] -> [a]
preCrescente2 [] = [] 
preCrescente2 [a] = [a]
preCrescente2 (x:y:xs)
                   |x<y =  x: preCrescente2 (y:xs)
                   | otherwise = [x] 
