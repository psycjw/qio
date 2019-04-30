import QIO.Qio
import QIO.QioSyn

import System.TimeIt

{-
Executes Grover's algorithm over a custom inputted oracle function, with any amount of solutions, whether known or unknown.

After receiving the amount of variables in the function, and the function inputted in the described form, the program will
create a list of "predicates", where each predicate consists of a list of integers in the corresponding line of the function,
where NOT-ted variables are negative.

Next, the amount of solutions is asked for, and results in 3 cases.
1. It is known that there is 1 solution to the function.
2. It is known that there is more than 1 solution to the function.
3. The amount of solutions to the function is not known.

Case 1. 1 solution
Run Grover's algorithm with "iterations n 1" iterations.

Case 2. >1 solutions, s
Run Grover's algorithm with "iterations n s" iterations, repeating until the desired amount of solutions, k where 0 < k <= s, is found.

Case 3. Unknown amount of solutions.
Run Grover's algorithm with increasing amounts of iterations, equal to "iterations n 1", "iterations n 2", "iterations n 4",
"iterations n 8",... up to (pi/4)(N^1/2)(2 + 2^1/2) iterations.
-}
main :: IO ()
main = do
  putStrLn "How many variables?"
  inV <- getLine
  let n = read inV :: Int
  putStrLn "Input each predicate with lines of AND operations, where each line will be OR'd, with an empty line after the last."
  putStrLn "For example, (x1 & -x2) | x1 | x2 would be inputted as\n1 -2\n1\n2\n\n-----"
  inP <- getLines
  let ps =  filter (not . null) $ map (filter (\p -> abs p <= n || p /= 0)) inP
  putStrLn "How many solutions does this formula have? This is used to calculate how many Grover iterations are needed. Input '?' if the amount of solutions is not known."
  inS <- getLine
  if inS == "?"
    then timeIt $ getUnknownSolution n 0 ps
    else do
      let s = read inS :: Int
      putStrLn $ "\nProbability of finding a solution after " ++ (show $ iterations n s) ++ " iteration(s) is: "
      probability n (iterations n s) ps
      timeIt $ solve n s ps
  where
    getLines = do
      l <- getLine
      if l == "" then return []
        else do
          let x = map read $ words l :: [Int]
          xs <- getLines
          return (x:xs)

solve :: Int -> Int -> [[Int]] -> IO ()
solve n s ps =
  if s == 1
    then do
      putStrLn "Search time:"
      out <- timeIt $ run $ grover n (iterations n 1) ps
      if head out
        then do
          putStrLn "Found solution:"
          putStrLn $ show $ tail out
          putStrLn "\nTotal time:"
        else do
          putStrLn "Found non-solution:"
          putStrLn $ show $ tail out
          solve n 1 ps
    else do
      putStrLn $ "\nHow many solutions do you want out of " ++ (show s) ++ "?"
      inK <- getLine
      let k = read inK :: Int
      if (k < 1 || k > s)
        then getSolutions n s ps [] s
        else getSolutions n s ps [] k

getSolutions :: Int -> Int -> [[Int]] -> [[Bool]] -> Int -> IO ()
getSolutions n s ps bs k = do
  if (k > 0)
    then do
      putStrLn "\nSearch time:"
      out <- timeIt $ run $ grover n (iterations n s) ps
      if head out
        then if (tail out) `notElem` bs
            then do
              putStrLn "Found new solution:"
              putStrLn $ show $ tail out
              getSolutions n s ps (bs ++ [tail out]) (k-1)
            else do
              putStrLn "Found old solution:"
              putStrLn $ show $ tail out
              getSolutions n s ps bs k
        else do
          putStrLn "Found non-solution:"
          putStrLn $ show $ tail out
          getSolutions n s ps bs k
    else do
      putStrLn "\nSolution(s):"
      mapM_ print bs
      putStrLn "\nTotal time:"

getUnknownSolution :: Int -> Int -> [[Int]] -> IO ()
getUnknownSolution n s ps = do
  if s >= n || i >= round ((pi * sqrt(2^(fromIntegral n))/4) * (2 + sqrt(2)))
    then putStrLn "No solutions found."
    else do
      putStrLn "\nSearch time:"
      out <- timeIt $ run $ grover n i ps
      if head out
        then do
          putStrLn "Found solution:"
          putStrLn $ show $ tail out
          putStrLn "\nTotal time:"
        else do
          putStrLn "Found non-solution:"
          putStrLn $ show $ tail out
          getUnknownSolution n (s+1) ps
  where
    i = iterations n (2^s)

iterations :: Int -> Int -> Int
iterations n s = round $ (pi/4) * sqrt((fromIntegral (2^n))/(fromIntegral s))

grover :: Int -> Int -> [[Int]] -> QIO [Bool]
grover n i ps = do
  (qI, qA) <- initialize n ((length ps)+1)
  let groverStep = oracle ps qI <> diffuse qI
  applyU (foldr (<>) mempty (replicate i groverStep))
  applyU (testForSolution 0 ps qI qA)
  measQbits ([last qA] ++ qI)

probability :: Int -> Int -> [[Int]] -> IO ()
probability n i ps = putStrLn $ show $ sim $ probOfSolution
  where
    probOfSolution = do
      (qI, qA) <- initialize n ((length ps)+1)
      let groverStep = oracle ps qI <> diffuse qI
      applyU (foldr (<>) mempty (replicate i groverStep))
      applyU (testForSolution 0 ps qI qA)
      measQbit $ last qA

oracle :: [[Int]] -> [Qbit] -> U
oracle [] _      = mempty
oracle (p:ps) qI =
  unitaryN unot (unots p qI) <>
  condN (\q -> uphase q pi) (conds p qI) <>
  unitaryN unot (unots p qI) <>
  oracle ps qI
  where
    unots p qI = map (\x -> qI!!((abs x)-1)) $ filter (< 0) p
    conds p qI = map (\x -> qI!!((abs x)-1)) p

testForSolution :: Int -> [[Int]] -> [Qbit] -> [Qbit] -> U
testForSolution _ [] _ qA      =
  unitaryN unot qA <>
  condN unot qA <>
  unitaryN unot (init qA)
testForSolution c (p:ps) qI qA =
  unitaryN unot (unots p qI) <>
  condN unot ((conds p qI) ++ [qA!!c]) <>
  unitaryN unot (unots p qI) <>
  testForSolution (c+1) ps qI qA
  where
    unots p qI = map (\x -> qI!!((abs x)-1)) $ filter (< 0) p
    conds p qI = map (\x -> qI!!((abs x)-1)) p

initialize :: Int -> Int -> QIO ([Qbit], [Qbit])
initialize i a = do
  qI <- mkQbits i True
  qA <- mkQbits a False
  applyU (unitaryN uhad qI)
  return (qI, qA)

diffuse :: [Qbit] -> U
diffuse qI =
  unitaryN uhad (init qI) <>
  condN unot qI <>
  unitaryN uhad (init qI)

mkQbits :: Int -> Bool -> QIO [Qbit]
mkQbits n b = mkQbits' n b []
  where
    mkQbits' 0 _ qs = return qs
    mkQbits' n b qs = do
      q <- mkQbit b
      mkQbits' (n-1) b (qs ++ [q])

measQbits :: [Qbit] -> QIO [Bool]
measQbits qs = measQbits' qs []
  where
    measQbits' [] bs = return bs
    measQbits' (q:qs) bs = do
      b <- measQbit q
      measQbits' qs (bs ++ [b])

unitaryN :: (Qbit -> U) -> [Qbit] -> U
unitaryN _ []      = mempty
unitaryN uf (q:qs) = uf q <> unitaryN uf qs

condN :: (Qbit -> U) -> [Qbit] -> U
condN uf (q:[]) = uf q
condN uf (q:qs) = cond q (\x -> if x then (condN uf qs) else mempty)
