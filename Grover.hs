import Data.Monoid
import QIO.Qio
import QIO.QioSyn
import QIO.QioClass
import QIO.QExamples

getSolutions :: Int -> [[Bool]] -> IO ()
getSolutions n bs =
    if (n>0) then
      do
        out <- run grover
        if head out && (tail out) `notElem` bs then
          getSolutions (n-1) (bs ++ [tail out])
        else
          getSolutions n bs
    else
      mapM_ print bs

{-
Chance returns the probability that the qubits q1, q2, q3 and q4 contain
a potential solution to the oracle.
-}
chance :: QIO Bool
chance =
  do
    q1 <- qMinus
    q2 <- qMinus
    q3 <- qMinus
    q4 <- qMinus
    qo1 <- mkQbit False
    qo2 <- mkQbit False
    qo3 <- mkQbit False
    qo4 <- mkQbit False
    qZ <- mkQbit False
    applyU (groverStep q1 q2 q3 q4 qo1 qo2 qo3 qo4 qZ)
    applyU (groverStep q1 q2 q3 q4 qo1 qo2 qo3 qo4 qZ)
    applyU (halfOracleStep q1 q2 q3 q4 qo1 qo2 qo3 qo4 qZ <>
            uZZ qZ)
    measQbit qZ

{-
Grover usually returns a solution to the oracle in the form of
[Bool], where the first Bool is true iff the remaining Bools form a solution.
-}
grover :: QIO [Bool]
grover =
  do
    q1 <- qMinus
    q2 <- qMinus
    q3 <- qMinus
    q4 <- qMinus
    qo1 <- mkQbit False
    qo2 <- mkQbit False
    qo3 <- mkQbit False
    qo4 <- mkQbit False
    qZ <- mkQbit False
    applyU (groverStep q1 q2 q3 q4 qo1 qo2 qo3 qo4 qZ)
    applyU (groverStep q1 q2 q3 q4 qo1 qo2 qo3 qo4 qZ)
    applyU (halfOracleStep q1 q2 q3 q4 qo1 qo2 qo3 qo4 qZ <>
            uZZ qZ)
    r1 <- measQbit q1
    r2 <- measQbit q2
    r3 <- measQbit q3
    r4 <- measQbit q4
    r5 <- measQbit qZ
    return [r5, r1, r2, r3, r4]

{-
groverStep performs 1 Grover step, i.e.
  Executes the oracle, creating a solution and rotating the solution state by 180o
  Executes the inverse of the oracle
  Diffuses the qubits (inverting amplitude around the average), enhancing the solution state's amplitude
-}
groverStep :: Qbit -> Qbit -> Qbit -> Qbit -> Qbit -> Qbit -> Qbit -> Qbit -> Qbit -> U
groverStep q1 q2 q3 q4 qo1 qo2 qo3 qo4 qZ =
    halfOracleStep q1 q2 q3 q4 qo1 qo2 qo3 qo4 qZ <>
    uZZ qZ <>
    urev (halfOracleStep q1 q2 q3 q4 qo1 qo2 qo3 qo4 qZ) <>
    diffuse q1 q2 q3 q4

-- halfOracleStep just executes the oracle and stores its result in a qubit
halfOracleStep :: Qbit -> Qbit -> Qbit -> Qbit -> Qbit -> Qbit -> Qbit -> Qbit -> Qbit -> U
halfOracleStep q1 q2 q3 q4 qo1 qo2 qo3 qo4 qZ =
  oracle q1 q2 q3 q4 qo1 qo2 qo3 qo4 <>
  unot qo1 <> unot qo2 <> unot qo3 <> unot qo4 <>
  condXn [qo1,qo2,qo3,qo4,qZ] <>
  unot qo1 <> unot qo2 <> unot qo3 <> unot qo4

{-
oracle executes the oracle on the qubits.

The current oracle corresponds to

(-x1 V -x3 V -x4) &
(x2 V x3 V -x4) &
(x1 V -x2 V x4) &
(-x1 V x2 V -x3)

Solutions

1 2 3 4
--------
0 0 0 0
0 0 1 0
0 0 1 1
0 1 0 1
0 1 1 1
1 0 0 0
1 1 0 0
1 1 0 1
1 1 1 0
-}
oracle :: Qbit -> Qbit -> Qbit -> Qbit -> Qbit -> Qbit -> Qbit -> Qbit -> U
oracle q1 q2 q3 q4 qo1 qo2 qo3 qo4 =
  condXn [q1, q3, q4, qo1] <>
  unot q2 <> unot q3 <>
  condXn [q2, q3, q4, qo2] <>
  unot q2 <> unot q3 <>
  unot q1 <> unot q4 <>
  condXn [q1, q2, q4, qo3] <>
  unot q1 <> unot q4 <>
  unot q2 <>
  condXn [q1, q2, q3, qo4] <>
  unot q2

diffuse :: Qbit -> Qbit -> Qbit -> Qbit -> U
diffuse q1 q2 q3 q4 =
  uhad q1 <> uhad q2 <> uhad q3 <>
  condXn [q1, q2, q3, q4] <>
  uhad q1 <> uhad q2 <> uhad q3

-- performs a c^nNot gate over n qubits, storing the result in the last qubit given
condXn :: [Qbit] -> U
condXn (q1:q2:[]) = cond q1 (\x -> if x then unot q2 else mempty)
condXn (q:qs) = cond q (\x -> if x then (condXn qs) else mempty)
