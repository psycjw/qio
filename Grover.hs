import Data.Monoid
import System.Time
import Text.Printf

import QIO.Qio
import QIO.QioSyn
import QIO.QioClass
import QIO.QExamples

main :: IO ()
main = do
  putStrLn "How many variables?"
  inV <- getLine
  let n = (read inV :: Int)
  putStrLn "Input each predicate with lines of AND operations, where each line will be OR'd, with an empty line after the last."
  putStrLn "For example, (x1 & -x2) | x1 | x2 would be inputted as\n1 -2\n1\n2\n\n-----"
  inP <- getLines
  let ps =  filter (not . null) $ map (removeOOB n) inP
  putStrLn "How many solutions does this formula have? This is used to calculate how many Grover iterations are needed to get an accurate answer."
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

For any s > 1, then Grover's algorithm needs to be run to get as many solutions as desired
by the user, and each time the amount of iterations needed is (pi/4)(N/s)^1/2.
-}
getSolution :: Int -> Int -> [[Int]] -> IO ()
getSolution s n ps =
  if s == 1 then do
    out <- run $ customGrover (calcIterations (2^n) 1) n ps
    print out
  else do
    printf "How many solutions do you want out of %d.\n" s
    inK <- getLine
    let k = (read inK :: Int)
    if (k < 0 || k > s) then
      getSolutions s [] n ps s
    else
      getSolutions s [] n ps k

getSolutions :: Int -> [[Bool]] -> Int -> [[Int]] -> Int -> IO ()
getSolutions s bs n ps k =
  if (k > 0) then do
    out <- run $ customGrover (calcIterations (2^n) s) n ps
    if out `notElem` bs then
      getSolutions s (bs ++ [out]) n ps (k-1)
    else
      getSolutions s bs n ps k
  else
    mapM_ print bs

customGrover :: Int -> Int -> [[Int]] -> QIO [Bool]
customGrover i n ps = do
  qI <- mkQbits n False
  qO <- mkQbits (length ps) True
  applyU (uhadN qI <> uhadN qO)
  applyU (groverSteps i ps qI qO)
  measQbits qI

groverSteps :: Int -> [[Int]] -> [Qbit] -> [Qbit] -> U
groverSteps i ps qI qO =
  foldr (<>) mempty (replicate i (grover ps qI qO))

grover :: [[Int]] -> [Qbit] -> [Qbit] -> U
grover ps qI qO =
  customOracle 0 ps qI qO <>
  diffuse qI

customOracle :: Int -> [[Int]] -> [Qbit] -> [Qbit] -> U
customOracle _ [] _ _ = mempty
customOracle n (p:ps) qI qO =
  unotN (unots p qI) <>
  condXn (conds p qI ++ [qO!!n]) <>
  unotN (unots p qI) <>
  customOracle (n+1) ps qI qO
  where
    unots p qI = map (\x -> qI!!((abs x)-1)) $ filter (< 0) p
    conds p qI = map (\x -> qI!!((abs x)-1)) p

calcIterations :: Int -> Int -> Int
calcIterations n s = ceiling $ (pi/4) * sqrt((fromIntegral n)/(fromIntegral s))

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
  uhadN qI <>
  unotN qI <>
  condZn qI <>
  unotN qI <>
  uhadN qI

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

condZn :: [Qbit] -> U
condZn (ql:[]) = uphase ql pi
condZn (q:qs) = cond q (\x -> if x then (condZn qs) else mempty)

-- performs multiple unot gates over each qubit input
unotN :: [Qbit] -> U
unotN [] = mempty
unotN (q:qs) = unot q <> unotN qs

-- performs multiple hadamard gates over each qubit input
uhadN :: [Qbit] -> U
uhadN [] = mempty
uhadN (q:qs) = uhad q <> uhadN qs
