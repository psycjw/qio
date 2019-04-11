module Circuit where

import Data.List
import Data.Maybe

import qualified QIO.QioSyn as Q

-- Gates are made up of the gate type, a Column index and a list of qubits it acts on, where (init qubits) are the control qubits
data Gate = Null
          | Had Int [Int]
          | PX Int [Int]
          | PY Int [Int]
          | PZ Int [Int]
          | Swap Int [Int]
          deriving (Eq, Show)

-- Gates are compared by their column index, allowing easy sorting around a column index.
instance Ord Gate where
  compare g1 g2 = compare (getCol g1) (getCol g2)

data Circuit = Circuit { qubits :: [Int],
                         gates :: [Gate],
                         edit :: String,
                         mouse :: (Float, Float),
                         result :: String
                       } deriving (Eq, Show)

initialCircuit :: Circuit
initialCircuit = Circuit { qubits = [0..2],
                           gates = [],
                           edit = "",
                           mouse = (0, 0),
                           result = "" }

getType :: Gate -> Int -> String
getType Null _        = ""
getType (Had _ qs) _  = "H" ++ concat (map (\q' -> " " ++ show q') $ init qs)
getType (PX _ qs) _   = "X" ++ concat (map (\q' -> " " ++ show q') $ init qs)
getType (PY _ qs) _   = "Y" ++ concat (map (\q' -> " " ++ show q') $ init qs)
getType (PZ _ qs) _   = "Z" ++ concat (map (\q' -> " " ++ show q') $ init qs)
getType (Swap _ qs) q = "Swap" ++ concat (map (\q' -> " " ++ show q') $ filter (/= q) qs)

getCol :: Gate -> Int
getCol Null       = -1
getCol (Had c _)  = c
getCol (PX c _)   = c
getCol (PY c _)   = c
getCol (PZ c _)   = c
getCol (Swap c _) = c

lteCol :: Int -> Gate -> Bool
lteCol _ Null         = False
lteCol c (Had cg _)   = cg <= c
lteCol c (PX cg _)    = cg <= c
lteCol c (PY cg _)    = cg <= c
lteCol c (PZ cg _)    = cg <= c
lteCol c (Swap cg _)  = cg <= c

incCol :: Gate -> Gate
incCol Null         = Null
incCol (Had c qs)   = Had (c+1) qs
incCol (PX c qs)    = PX (c+1) qs
incCol (PY c qs)    = PY (c+1) qs
incCol (PZ c qs)    = PZ (c+1) qs
incCol (Swap c qs)  = Swap (c+1) qs

decCol :: Gate -> Gate
decCol Null         = Null
decCol (Had c qs)   = Had (c-1) qs
decCol (PX c qs)    = PX (c-1) qs
decCol (PY c qs)    = PY (c-1) qs
decCol (PZ c qs)    = PZ (c-1) qs
decCol (Swap c qs)  = Swap (c-1) qs

getQs :: Gate -> [Int]
getQs Null        = [-1]
getQs (Had c qs)  = qs
getQs (PX c qs)   = qs
getQs (PY c qs)   = qs
getQs (PZ c qs)   = qs
getQs (Swap c qs) = qs

addQubit :: Circuit -> Circuit
addQubit circuit =
  if n < 10
    then circuit { qubits = (qubits circuit) ++ [n] }
    else circuit
  where
    qs = qubits circuit
    n  = length qs

remQubit :: Circuit -> Circuit
remQubit circuit = let qs = qubits circuit in
                     if length qs > 0
                       then circuit { qubits = init qs, gates = filter (/= Null) $ map (remGateOnQubit (last qs)) (gates circuit) }
                       else circuit

remGateOnQubit :: Int -> Gate -> Gate
remGateOnQubit q g = if q `elem` (getQs g)
                       then Null
                       else g

pickUpGate :: Int -> Int -> Circuit -> Circuit
pickUpGate c q circuit =
  case getGate c q circuit of
    Null -> circuit
    g    -> circuit { gates = delete g $ gates circuit,
                      edit = getType g q }

addGate :: Float -> Int -> Circuit -> Circuit
addGate c q circuit =
  if c' > 0.25 && c' <= 0.75
    then addNewColGate c q circuit
    else if slotFull (round c) q circuit || newG == Null
           then circuit
           else circuit { gates = (gates circuit) ++ [newG],
                          edit = "" }
  where
    c'   = c - (fromInteger $ floor c)
    newG = newGate (round c) q circuit

addNewColGate :: Float -> Int -> Circuit -> Circuit
addNewColGate c q circuit =
  case newG of
    Null -> circuit
    g    -> circuit { gates = bG ++ [g] ++ (map incCol aG),
                      edit = "" }
  where
    (bG, aG) = span (lteCol (floor c)) $ sort $ gates circuit
    newG     = newGate ((ceiling c)) q circuit

slotFull :: Int -> Int -> Circuit -> Bool
slotFull c q circuit = any (\q' -> anyGate q') $ getControls $ edit circuit
  where
    anyGate q'' = any (\g -> q'' `elem` getQs g && c == getCol g) $ gates circuit

getGate :: Int -> Int -> Circuit -> Gate
getGate c q circuit = fromMaybe Null $ find (\g -> q `elem` getQs g && c == getCol g) $ gates circuit

newGate :: Int -> Int -> Circuit -> Gate
newGate c q circuit =
  case head e of
    'H'       -> if qInCqs || anyCq
                   then Null
                   else Had c qs
    'X'       -> if qInCqs || anyCq
                   then Null
                   else PX c qs
    'Y'       -> if qInCqs || anyCq
                   then Null
                   else PY c qs
    'Z'       -> if qInCqs || anyCq
                   then Null
                   else PZ c qs
    'S'       -> if q == q2 || q2 >= (length $ qubits circuit)
                   then Null
                   else Swap c [q, q2]
    otherwise -> Null
  where
    e      = edit circuit
    q2     = read [last e] :: Int
    cqs    = getControls e
    qs     = cqs ++ [q]
    qInCqs = q `elem` cqs
    anyCq  = any (\cq -> cq >= (length $ qubits circuit)) cqs

getControls :: String -> [Int]
getControls = map (\c -> (read [c] :: Int)) . nub . filter (/= ' ') . dropWhile (/= ' ')
