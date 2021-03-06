module QIO.Circuit.Logic where

import QIO.Circuit.Circuit
import QIO.Circuit.Render
import QIO.Circuit.Compile
import Graphics.Gloss.Interface.Pure.Game

import System.IO.Unsafe
import QIO.Qio

{-
Event
EventKey Key KeyState Modifiers (Float, Float)
    Key = Char | SpecialKey | MouseButton
    KeyState = Down | Up
    Modifiers = shift KeyState ctrl KeyState alt KeyState
EventMotion (Float, Float)
EventResize (Int, Int)
-}
handleEvent :: Event -> Circuit -> Circuit
handleEvent (EventKey (MouseButton LeftButton) Up _ pos) circuit = mousePosition pos circuit
handleEvent (EventKey (MouseButton RightButton) Up _ pos) circuit =
  if edit circuit /= ""
    then circuit { edit = "" }
    else circuit
handleEvent (EventMotion pos) circuit = circuit { mouse = pos }
handleEvent (EventKey (Char c) Up _ pos) circuit =
  case head e of
    'H' | c == '\b'                             -> circuit { edit = "H" }
    'H' | c >= '0' && c <= '9'                  -> circuit { edit = e ++ [' ', c] }
    'X' | c == '\b'                             -> circuit { edit = "X" }
    'X' | c >= '0' && c <= '9'                  -> circuit { edit = e ++ [' ', c] }
    'Y' | c == '\b'                             -> circuit { edit = "Y" }
    'Y' | c >= '0' && c <= '9'                  -> circuit { edit = e ++ [' ', c] }
    'Z' | c == '\b'                             -> circuit { edit = "Z"}
    'Z' | c >= '0' && c <= '9'                  -> circuit { edit = e ++ [' ', c] }
    'S' | c == '\b'                             -> circuit { edit = "Swap" }
    'S' | c >= '0' && c <= '9' && length e == 4 -> circuit { edit = e ++ [c] }
    otherwise                                   -> circuit
    where
      e = edit circuit
handleEvent (EventKey (SpecialKey KeyEnter) Up _ pos) circuit = circuit { result = show $ unsafePerformIO $ run $ compileCircuit circuit }
handleEvent _ circuit = circuit

{-
let leftx = 50, spacing = 100
add : (leftX - spacing/4) -> (leftX - spacing/4 + spacing/2)
sub : (leftX - spacing/4 + 2*spacing/2) -> (leftX - spacing/4 + 3*spacing/2)
had : (leftX - spacing/4 + 4*spacing/2) -> (leftX - spacing/4 + 5*spacing/2)

add : 25 -> 75
sub : 125 -> 175
had : 225 -> 275

floor((x - leftX + spacing/4)/(spacing/2)) : add (0 -> 1) | sub (2 -> 3) | had (4 -> 5)
25-74 -> 0
75-124 -> 1
125-174 -> 2
175-224 -> 3

c = 0: leftX + spacing - spacing/4 -> leftX + spacing + spacing/4
c = 1: leftX + 2*spacing - spacing/4 -> leftX + 2*spacing + spacing/4
c = 0 -> leftX + 1*spacing
c = 1 -> leftX + 2*spacing
c = 2 -> leftX + 3*spacing

q = topY - spacing - spacing/2 - q*spacing
0 = topY - spacing - spacing/2
1 = topY - spacing - spacing/2 - spacing
-}

index :: [String] -> Int -> String
index ss i =
  if i < 0 || i >= length ss
    then ""
    else ss !! i

mousePosition :: (Float, Float) -> Circuit -> Circuit
mousePosition (mX, mY) circuit
  | bX `mod` 2 == 0 && mY > bY && mY < bY + spacing/2                           = buttonPress (circuit { mouse = (mX, mY) }) $ index buttons $ bX `div` 2
  | edit circuit /= "" && eX >= -0.5 && eY >= 0 && eY < length (qubits circuit) = addGate eX eY circuit
  | edit circuit == "" && eX >= -0.5 && eY >= 0 && eY < length (qubits circuit) = pickUpGate (round eX) eY circuit
  | otherwise                                                                   = circuit
  where
    bX = floor $ (mX - leftX + spacing/4)/(spacing/2)     :: Int
    bY = height/2 - 3*spacing/4
    eX = ((mX - leftX)/(spacing))-1
    eY = floor $ (mY - topY + spacing)/(-spacing)         :: Int

buttonPress :: Circuit -> String -> Circuit
buttonPress circuit "+"     = addQubit circuit
buttonPress circuit "-"     = remQubit circuit
buttonPress circuit "Run"   = circuit { result = show $ unsafePerformIO $ run $ compileCircuit circuit }
buttonPress circuit "Sim"   = circuit { result = show $ sim $ compileCircuit circuit }
buttonPress circuit "Clear" = circuit { gates = [], edit = "", result = "" }
buttonPress circuit s       = circuit { edit = s }
