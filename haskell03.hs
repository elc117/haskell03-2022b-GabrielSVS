add10toall :: [Int] -> [Int]
add10toall x = [x+10 | x <- x]

multN :: Int -> [Int] -> [Int]
multN n list = [n*list | list <- list]

multN' :: Int -> [Int] -> [Int]
multN' x list = map (*x) list

applyExpr :: [Int] -> [Int]
applyExpr x = [3*x+2 | x <- x]

applyExpr' :: [Int] -> [Int]
applyExpr' x = map (\x -> 3*x+2) x

addSuffix :: String -> [String] -> [String]
addSuffix x list = [string ++ x | string <- list]

selectgt5 :: [Int] -> [Int]
selectgt5 x = [x | x <- x , x > 5]

sumOdds :: [Int] -> Int
sumOdds list = sum [list | list <- list , odd list]

sumOdds' :: [Int] -> Int
sumOdds' list = sum (filter odd list)

selectExpr :: [Int] -> [Int]
selectExpr list = [list | list <- list , even list && list >=20 && list <=50]

countShorts :: [String] -> Int
countShorts str = length [str | str <- str , length str < 5]

calcExpr :: [Float] -> [Float]
calcExpr x = [x^2/2 | x <- x , (x^2/2)>10]

trSpaces :: String -> String
trSpaces str = concat [if elem x " " then ['-'] else [x] | x <- str]

selectSnd :: [(Int,Int)] -> [Int]
selectSnd t = [y | (x,y) <- t]

dotProd :: [Int] -> [Int] -> Int
dotProd x y = sum [x*y | (x,y) <- zip x y]