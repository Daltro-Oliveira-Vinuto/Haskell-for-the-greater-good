import Data.Char

answer :: Int
answer = 10
value :: Int 
value = 100

greater :: Bool
greater = (value > 7)

yes :: Bool  
yes = True

square :: Int -> Int 
square x = x*x 

allEqual :: Int -> Int -> Int -> Bool
allEqual a b c = (a == b) && (b == c)

maxIn2 :: Int -> Int -> Int
maxIn2 a b  | a >= b = a 
            | otherwise = b 


addD :: Int -> Int -> Int 
addD a b = 2*(a + b)

--answer = (addD 2 (addD 3 4))

-- factorial program
factorial :: Int -> Int 
factorial n | (n <= 1) = 1
            | otherwise = n*factorial (n-1)

--answer = factorial 5  

all4Equal :: Int -> Int -> Int -> Int -> Bool  
all4Equal a b c d  = (allEqual a b c) && (allEqual a b d) 

--answer = (all4Equal 2 2 2 3)

howManyEqual :: Int -> Int -> Int -> Int 
howManyEqual a b c | allEqual a b c = 3
                   | (a == b) || (a == c) || (b == c) = 2  
                   | otherwise = 1      

--answer = (howManyEqual 1 2 3)

sales :: Int -> Int 
--sales n | n /= 0 =  100 `div` n
--       | otherwise = 0 
--sales n = 10
sales n | n == 0 = 12
        | n == 1 = 23
        | n == 2 = 17

--totalSales :: Int -> Int 
--totalSales n | n == 0 = sales 0  
--             | otherwise = sales n + totalSales (n-1)

--answer = totalSales 5

totalSales :: Int -> Int 
totalSales 0 = sales 0  
totalSales n = sales n + totalSales (n-1)

maxOf2 :: Int -> Int -> Int 
maxOf2 a b  | a >= b = a 
            | otherwise = b

--maxSales :: Int -> Int 
--maxSales n  | n == 0 = sales 0 
--            | otherwise = maxOf2 (sales n) (maxSales (n-1))

maxSales :: Int -> Int 
maxSales 0 = sales 0 
maxSales n = maxOf2 (sales n) (maxSales (n-1)) 

--answer = maxSales 5

myNot :: Bool -> Bool 
myNot True = False 
myNot False = True 

myOr :: Bool -> Bool -> Bool
myOr True x = True 
myOr False x = x 

myAnd :: Bool -> Bool -> Bool 
myAnd True x = x 
myAnd False x = False 

{-
getSalesOccorrences :: Int -> Int -> Int 
getSalesOccorrences n s | (n == 0) && (sales n == s) = 1
                        | (n == 0) && (sales n /= s) = 0
                        | (n /=  0) && (sales n == s) = 1 + getSalesOccorrences (n-1) s
                        | (n /= 0) && (sales n /= s) = getSalesOccorrences (n-1) s

-}

getSalesOccorrences :: Int -> Int -> Int 
getSalesOccorrences 0 s | (sales 0 == s) = 1
                        | (sales 0 /= s) = 0

getSalesOccorrences n s | (sales n == s) = 1 + getSalesOccorrences (n-1) s
                        |  (sales n /= s) = getSalesOccorrences (n-1) s


offSet :: Int 
offSet = ord 'a' - ord 'A'

capitalize :: Char -> Char 
capitalize ch = chr (ord ch - offSet)

myIsDigit :: Char -> Bool 
myIsDigit ch = (ch >= '0') && (ch <= '9')

makeSpaces :: Int -> String 
makeSpaces 0 = ""
makeSpaces n = " " ++ makeSpaces (n-1)

pushRight :: Int -> String -> String 
pushRight 0 s = s 
pushRight n s = makeSpaces n ++ s 

averageSales :: Int -> Float 
averageSales 0 = fromIntegral (sales 0)
averageSales n = fromIntegral (totalSales n) / fromIntegral n 

intTuple :: (Int, Int)
intTuple = (40, 50)

addPair :: (Int, Int) -> Int 
addPair (x, y) = x+y 

shift :: ((int,int),int) -> (int,(int,int))
shift ((x,y),z) = (x, (y, z))

{-
type Name = String 
type Age = Int 
type Phone = Int 
type Person = (Name, Age, Phone)

name :: Person -> Name 
name (n, a, p) = n
-}

sumSquares1 :: Int -> Int -> Int 
sumSquares1 x y = sqrX + sqrY  
                  where sqrX  = x*x
                        sqrY  = y*y

sumSquares2 :: Int -> Int -> Int 
sumSquares2 x y = let sqrX = x*x  
                      sqrY = y*y 
                  in sqrX  + sqrY

{-
maxOfThree :: Int -> Int -> Int -> Int 
maxOfThree  a b c | (a >= b) && (a >= c) = a  
                  | (b >= a) && (b >= c) = b  
                  | (c >= a) && (c >= b) = c  
                  | otherwise = a 
-}

{-
eqCount :: Int -> Int -> Int -> Int -> Int 
eqCount x a b c | (cmp2 x a) && (cmp2 x b) && (cmp2 x c) = 3 
                | ((cmp2 x a) && (cmp2 x b))||((cmp2 x a) && (cmp2 x c))||((cmp2 x b) && (cmp2 x c)) = 2
                | (cmp2 x a) || (cmp2 x b) || (cmp2 x c) = 1
                | otherwise = 0
                where cmp2 a b = (a == b) 
-}

maxThreeOccurs :: Int -> Int -> Int -> (Int, Int)
maxThreeOccurs a b c = (mx, mxCount)
                    where mx = (maxOfThree a b c) 
                          mxCount = (eqCount mx a b c)
                          maxOfThree  a b c | (a >= b) && (a >= c) = a  
                                            | (b >= a) && (b >= c) = b  
                                            | (c >= a) && (c >= b) = c  
                                            | otherwise = a 
                          eqCount x a b c  | (cmp2 x a) && (cmp2 x b) && (cmp2 x c) = 3 
                                           | ((cmp2 x a) && (cmp2 x b))||((cmp2 x a) && (cmp2 x c))||((cmp2 x b) && (cmp2 x c)) = 2
                                           | (cmp2 x a) || (cmp2 x b) || (cmp2 x c) = 1
                                           | otherwise = 0
                                            where cmp2 a b = (a == b) 


printTable :: Int -> String 
printTable n = heading ++
               printWeeks ++
               printTotal ++
               printAverage

            where heading = ("Week" ++ makeSpaces 5 ++ "Sales" )
                  printWeeks = ( "" ++ "")
                  printTotal = ("Total" ++ makeSpaces 5 ++ show (totalSales n) ++"")
                  printAverage = ("Average" ++ makeSpaces 5 ++ show (averageSales n) )




oneRoot :: Float -> Float -> Float -> Float 
oneRoot a b c = (-b) / (2.0*a)

twoRoots :: Float -> Float -> Float -> (Float, Float)
twoRoots a b c = (d+e, d-e)
                where d = (-b)/(2.0*a)
                      e = sqrt(b^2 - 4.0*a*c)/(2.0*a)

roots :: Float -> Float -> Float -> String 
roots a b c | numberOfRoots == 2 = show x ++ " " ++ show y
            | numberOfRoots == 1 = show (oneRoot a b c )
            | otherwise = "no roots" 
            
            where (x,y) = twoRoots a b c 
                  numberOfRoots | (b^2 - 4.0*a*c) > 0 = 2 
                                | (b^2 - 4.0*a*c) == 0 = 1
                                | otherwise = 0


sumList :: [Int] -> Int 
sumList [] = 0
sumList (a:as) = a + sumList as 


double :: [Int] -> [Int]
double [] = []
double (a:as) = (2*a) : double as

member :: [Int] -> Int -> Bool 
member [] x = False 
member (a:as) x | x == a = True 
                | otherwise = member as x


digits :: String -> String
digits [] = ""
digits (a:as) | a >= '0' && a <= '9' = a : digits as 
              | otherwise = digits as 


sumPairs :: [(Int, Int)] -> [Int]
sumPairs [] = []
sumPairs ((a,b):as) = (a+b) : sumPairs as 

firstDigit :: String -> Char  
firstDigit s  = case (digits s) of  
                    [] -> '\0'
                    (a:as) -> a 


myLength :: [t] -> Int 
myLength [] = 0
myLength (a:as) = 1 + myLength as 

conc :: [t] -> [t] -> [t]
[] `conc` y = y 
(a:as) `conc` y = a:(as `conc` y)

myZip :: [a] -> [b] -> [(a,b)]
myZip [] [] = []
myZip (a:as) [] = []
myZip [] (b:bs) = []
myZip (a:as) (b:bs) = (a,b) : myZip as bs 


first :: (a,b) -> a 
first (a,b) = a

second :: (a,b) -> b 
second (a,b) = b


myHead :: [t] -> t 
myHead (a:as) = a 

myTail :: [t] ->[t]
myTail [] = []
myTail (a:as) = as 

type Person = String 
type Book = String 
type Database = [(Person, Book)]

exampleBase :: Database = [("Alice", "Postman Pat"),
                           ("Anna", "All Alone"),
                           ("Alice", "Spot"),
                           ("Rory", "Postman Pat")]


books :: Database -> Person -> [Book]
books [] _ = []
books ((p,b):as) k | (p == k) = b : books as k 
                   | otherwise = books as k 

borrowers :: Database -> Book -> [Person]
borrowers [] _ = []
borrowers ((p,b):as) k | b == k = p : borrowers as k 
                       | otherwise  = borrowers as k 

borrowed :: Database -> Book -> Bool 
borrowed [] _ = False 
borrowed ((p,b):as) k | b == k = True 
                      | otherwise = borrowed as k 

numBooksBorrowed :: Database -> Person -> Int 
numBooksBorrowed [] _ = 0 
numBooksBorrowed ((p,b):as) k | p == k = 1 + numBooksBorrowed as k
                              | otherwise = numBooksBorrowed as k
















main = print (answer)