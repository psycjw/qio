module Compile where

import qualified Circuit as C

import Data.Complex
import QIO.QioSyn
import QIO.Qio

compileCircuit :: C.Circuit -> QIO [Bool]
compileCircuit circuit = do
  qs <- mkQbits (length $ C.qubits circuit) False
  applyU (gatesToU (C.gates circuit) qs)
  measQbits qs

gatesToU :: [C.Gate] -> [Qbit] -> U
gatesToU [] _      = mempty
gatesToU (g:gs) qs = (gateToU g qs) <> (gatesToU gs qs)

gateToU :: C.Gate -> [Qbit] -> U
gateToU (C.Had _ qis) qs         = condU uhad $ map (\i -> qs !! i) qis
gateToU (C.PX _ qis) qs          = condU unot $ map (\i -> qs !! i) qis
gateToU (C.PY _ qis) qs          = condU (\q -> rot q yRot) $ map (\i -> qs !! i) qis
                                   where
                                     yRot (False, True) = 0 :+ 1
                                     yRot (True, False) = 0 :+ (-1)
                                     yRot _             = 0
gateToU (C.PZ _ qis) qs          = condU (\q -> uphase q pi) $ map (\i -> qs !! i) qis
gateToU (C.Swap _ (q1:q2:[])) qs = swap (qs !! q1) (qs !! q2)
gateToU _ _                      = mempty

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

condU :: (Qbit -> U) -> [Qbit] -> U
condU u (ql:[]) = u ql
condU u (q:qs)  = cond q (\x -> if x then (condU u qs) else mempty)
