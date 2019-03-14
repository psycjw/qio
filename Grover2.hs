import Text.Printf
import Data.Monoid
import QIO.Qio
import QIO.QioSyn
import QIO.QioClass
import QIO.QExamples
import System.Time

{-
Example
-1 -3 -4
2 3 -4
1 -2 4
-1 2 -3

9 Solutions

1 2 3 4
-------
0 0 0 0.
0 0 1 0.
0 0 1 1.
0 1 0 1.
0 1 1 1.
1 0 0 0.
1 1 0 0.
1 1 0 1.
1 1 1 0.
-}

{-
The main method is used to input a custom boolean problem, taking in the amount of variables,
the boolean predicates and the amount of solutions to the problem (optional).
Grover's algorithm is then performed using an oracle created with the predicates inputted,
and solution(s) found are printed at the end. This is timed using the System.Time module.
-}
main :: IO ()
main = do
  putStrLn "How many variables?"
  inV <- getLine
  let n = (read inV :: Int)
  putStrLn "Input each predicate in its dimacs version of the clause normal form syntax on its own line, with an empty line after the last."
  putStrLn "For example, (-x1 V x2) & x1 & -x2 would be inputted as\n-1 2\n1\n-2\n\n-----"
  inP <- getLines
  let ps =  filter (not . null) $ map (removeOOB n) inP
  putStrLn "How many solutions does this formula have? Input ? if unknown. This is used to calculate how many Grover iterations are needed to get an accurate answer."
  inS <- getLine
  let s = if inS == "?" then -1 else (read inS :: Int)
  start <- getClockTime
  getSolution s n ps
  stop <- getClockTime
  putStrLn ("The total time taken was " ++ (timeDiffToString (diffClockTimes stop start)))
  where
    getLines = do
      l <- getLine
      if l == "" then return []
        else do
          let x = map read $ words l :: [Int]
          xs <- getLines
          return (x:xs)

{-
getSolution's arguments are the number of solutions, s (-1 in the case of an unknown amount),
the number of variables, n, and the list of the predicates, ps.

If s is 1, then Grover's algorithm needs to be run once, with the amount of iterations needed
equalling (pi/4)(N^1/2) where N = 2^n.

If s is -1, then there could be any amount of solutions to the problem, and so it is
impossible to know how many iterations are necessary. Therefore, we must iterate for
increasing values of s, i.e.
(pi/4)(N^1/2), (pi/4)(N/2)^1/2, (pi/4)(N/4)^1/2, (pi/4)(N/8)^1/2....
From this, we can see the maximum number of iterations necessary is
(pi/4)(N^1/2)(1 + 2^(-1/2) + 4^(-1/2) + 8^(-1/2) + ...) = (pi/4)(N^1/2)(2 + 2^1/2) iterations.
If no solutions are found after this, then there are no solutions to the problem.

For any s > 1, then Grover's algorithm needs to be run to get as many solutions as desired
by the user, and each time the amount of iterations needed is (pi/4)(N/s)^1/2.
-}
getSolution :: Int -> Int -> [[Int]] -> IO ()
getSolution s n ps =
  if s == 1 then do
    out <- run $ customGrover (calcIterations (2^n) 1) n ps
    if head out then
      print $ tail out
    else
      getSolution 1 n ps
  else if s == -1 then
    getUnknownSolution 1 n ps
  else do
    printf "How many solutions do you want out of %d.\n" s
    inK <- getLine
    let k = (read inK :: Int)
    if (k < 0 || k > s) then
      getSolutions s [] n ps s
    else
      getSolutions s [] n ps k

getUnknownSolution :: Int -> Int -> [[Int]] -> IO ()
getUnknownSolution s n ps =
  if (i >= round ((pi * sqrt(2^(fromIntegral n))/4) * (2 + sqrt(2)))) then do
    putStrLn "No solutions found."
  else do
    printf "Performing Grover's algorithm with %d iterations.\n" i
    out <- run $ customGrover i n ps
    if head out then
      print $ tail out
    else
      getUnknownSolution (s+1) n ps
  where
    i = calcIterations (2^n) (2^s)

getSolutions :: Int -> [[Bool]] -> Int -> [[Int]] -> Int -> IO ()
getSolutions s bs n ps k =
  if (k > 0) then do
    out <- run $ customGrover (calcIterations (2^n) s) n ps
    if head out && (tail out) `notElem` bs then
      getSolutions s (bs ++ [tail out]) n ps (k-1)
    else
      getSolutions s bs n ps k
  else
    mapM_ print bs

customGrover :: Int -> Int -> [[Int]] -> QIO [Bool]
customGrover i n ps = do
  qI <- mkQbits n True
  applyU (uhadN qI)
  qO <- mkQbits (length ps) False
  qZ <- mkQbit False
  applyU (groverSteps i ps qI qO qZ)
  measQbits ([qZ] ++ qI)

groverSteps :: Int -> [[Int]] -> [Qbit] -> [Qbit] -> Qbit -> U
groverSteps i ps qI qO qZ =
  foldr (<>) mempty (replicate i (grover ps qI qO qZ))
  <> halfGrover ps qI qO qZ <>
  uZZ qZ

grover :: [[Int]] -> [Qbit] -> [Qbit] -> Qbit -> U
grover ps qI qO qZ =
  halfGrover ps qI qO qZ <>
  uZZ qZ <>
  urev (halfGrover ps qI qO qZ) <>
  diffuse qI

halfGrover :: [[Int]] -> [Qbit] -> [Qbit] -> Qbit -> U
halfGrover ps qI qO qZ =
  customOracle 0 ps qI qO <>
  unotN qO <>
  condXn (qO ++ [qZ]) <>
  unotN qO

customOracle :: Int -> [[Int]] -> [Qbit] -> [Qbit] -> U
customOracle _ [] _ _ = mempty
customOracle n (p:ps) qI qO =
  unotN (unots p qI) <>
  condXn (conds p qI ++ [qO!!n]) <>
  unotN (unots p qI) <>
  customOracle (n+1) ps qI qO
  where
    unots p qI = map (\x -> qI!!(x-1)) $ filter (> 0) p
    conds p qI = map (\x -> qI!!((abs x)-1)) p

calcIterations :: Int -> Int -> Int
calcIterations n s = round $ (pi/4) * sqrt((fromIntegral n)/(fromIntegral s))

removeOOB :: Int -> [Int] -> [Int]
removeOOB _ [] = []
removeOOB n ps =
  if (any (\p -> abs p > n || p == 0) ps)
    then []
    else ps

{-
When diffuse is called, a solution state stored in qI will have a
negative phase, while non-solution states will not. Diffuse increases the
amplitude of negative-phased states via an inversion around the mean amplitude,
effectively "picking" out solution states.
-}
diffuse :: [Qbit] -> U
diffuse qI =
  uhadN (init qI) <>
  condXn qI <>
  uhadN (init qI)

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

-- performs a c^nNot gate over n qubits, storing the result in the last qubit given
condXn :: [Qbit] -> U
condXn (ql:[]) = unot ql
condXn (q:qs) = cond q (\x -> if x then (condXn qs) else mempty)

-- performs multiple unot gates over each qubit input
unotN :: [Qbit] -> U
unotN [] = mempty
unotN (q:qs) = unot q <> unotN qs

-- performs multiple hadamard gates over each qubit input
uhadN :: [Qbit] -> U
uhadN [] = mempty
uhadN (q:qs) = uhad q <> uhadN qs
