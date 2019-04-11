module Render where

import Circuit
import Graphics.Gloss
import Graphics.Gloss.Data.Color

width, height, leftX, spacing, textSize :: Float
width = 1024
height = 720
leftX = spacing/1.5 - width/2
topY = height/2 - spacing/2
spacing = 55
textSize = spacing / 25

buttons :: [String]
buttons = [ "+",
            "-",
            "H",
            "X",
            "Y",
            "Z",
            "Swap",
            "Run",
            "Sim",
            "Clear" ]

draw :: Circuit -> Picture
draw circuit = translate leftX topY $
               pictures [ drawButtons,
                          translate 0 (-spacing) $ drawEditDots circuit,
                          translate 0 (-spacing) $ drawCircuit circuit,
                          translate (-leftX) (-topY) $ drawEdit circuit,
                          translate 0 (calcY (length $ qubits circuit) - spacing) $ color black $ scale 0.1 0.1 $ text $ result circuit ]

drawButtons :: Picture
drawButtons = pictures $
              (map (\(x, b) -> translate (x*spacing) 0 $ drawBox (-1) (-1) b) $ zip [0..] buttons) ++
              [ color black $ line [(-spacing/2, -spacing/2), (width-spacing, -spacing/2)],
                color black $ line [(-spacing/2, -spacing/2-1), (width-spacing, -spacing/2-1)],
                color black $ line [(3*spacing/2, spacing/3), (3*spacing/2, -spacing/3)],
                color black $ line [(13*spacing/2, spacing/3), (13*spacing/2, -spacing/3)]]

drawCircuit :: Circuit -> Picture
drawCircuit circuit = pictures [ drawQubits (qubits circuit),
                                 drawGates (gates circuit)]

drawQubits :: [Int] -> Picture
drawQubits = pictures . map drawQubit

drawQubit :: Int -> Picture
drawQubit q = color black $
              pictures [ translate (-spacing/2) (y-4) $ scale 0.1 0.1 $ text ("q" ++ (show q) ++ ": |0>"),
                         line [(spacing/2, y), (width, y)]]
              where
                y = calcY q

drawGates :: [Gate] -> Picture
drawGates = pictures . map drawGate

drawGate :: Gate -> Picture
drawGate Null                = Blank
drawGate (Had c qs)          = pictures $
                                 (map (\q -> color black $ translate (calcX c) (calcY q) $ circleSolid (textSize*1.5)) (init qs)) ++
                                 [ drawLines c qs [],
                                   drawBox c (last qs) "H" ]
drawGate (PX c qs)           = pictures $
                                 (map (\q -> color black $ translate (calcX c) (calcY q) $ circleSolid (textSize*1.5)) (init qs)) ++
                                 [ drawLines c qs [],
                                   gate ]
                               where
                                 ql = calcY $ last qs
                                 gate = if length qs == 1
                                          then drawBox c (head qs) "X"
                                          else pictures [ color black $ translate (calcX c) ql $ thickCircle (textSize*3) (textSize/1.3),
                                                          color black $ translate (calcX c) ql $ rectangleSolid (textSize*5.2) (textSize/2),
                                                          color black $ translate (calcX c) ql $ rectangleSolid (textSize/2) (textSize*5.2) ]
drawGate (PY c qs)           = pictures $
                                 (map (\q -> color black $ translate (calcX c) (calcY q) $ circleSolid (textSize*1.5)) (init qs)) ++
                                 [ drawLines c qs [],
                                   drawBox c (last qs) "Y" ]
drawGate (PZ c qs)           = pictures $
                                 (map (\q -> color black $ translate (calcX c) (calcY q) $ circleSolid (textSize*1.5)) (init qs)) ++
                                 [ drawLines c qs [],
                                   drawBox c (last qs) "Z" ]
drawGate (Swap c qs)         = pictures [ drawLines c [ql..qr] [],
                                          translate (calcX c) (calcY ql) $ drawText "x",
                                          translate (calcX c) (calcY qr) $ drawText "x" ]
                               where
                                 ql = minimum qs
                                 qr = maximum qs

drawEdit :: Circuit -> Picture
drawEdit circuit =
  if e /= ""
    then translate mX mY $ drawBox (-1) (-1) e
    else Blank
  where
    e = edit circuit
    mX = fst $ mouse circuit
    mY = snd $ mouse circuit

drawEditDots :: Circuit -> Picture
drawEditDots circuit =
  if edit circuit /= ""
    then pictures $ concat $ map (\c -> (map (\q -> color black $ translate (calcX c) (calcY q) $ circleSolid 2) $ qubits circuit)) [0..20]
    else Blank

drawLines :: Int -> [Int] -> [Picture] -> Picture
drawLines _ (q:[]) ps     = pictures ps
drawLines c (q1:q2:qs) ps = drawLines c (q2:qs) (ps ++ [color black $ line [(calcX c, calcY q1), (calcX c, calcY q2)]])

drawBox :: Int -> Int -> String -> Picture
drawBox (-1) (-1) t = pictures [ color white $ rectangleSolid (spacing/2) (spacing/2),
                                 color black $ rectangleWire (spacing/2) (spacing/2),
                                 drawText t ]
drawBox c q t       = translate (calcX c) (calcY q) $
                      pictures [ color white $ rectangleSolid (spacing/2) (spacing/2),
                                 color black $ rectangleWire (spacing/2) (spacing/2),
                                 drawText t ]

drawText :: String -> Picture
drawText "H"     = color black $
                   pictures [ translate (-textSize*1.5) 0 $ rectangleSolid textSize (textSize*5),
                              rectangleSolid (textSize*2) textSize,
                              translate (textSize*1.5) 0 $ rectangleSolid textSize (textSize*5) ]
drawText "X"     = color black $
                   pictures [ rotate (-45) $ rectangleSolid textSize (textSize*6),
                              rotate 45 $ rectangleSolid textSize (textSize*6) ]
drawText "Y"     = color black $
                   pictures [ translate 0 (-textSize*1.25) $ rectangleSolid textSize (textSize*3),
                              translate (-textSize) textSize $ rotate (-45) $ rectangleSolid textSize (textSize*2.5),
                              translate textSize textSize $ rotate 45 $ rectangleSolid textSize (textSize*2.5) ]
drawText "Z"     = color black $
                   pictures [ translate 0 (textSize*1.7) $ rectangleSolid (textSize*4) textSize,
                              rotate 45 $ rectangleSolid textSize (textSize*4.35),
                              translate 0 (-textSize*1.7) $ rectangleSolid (textSize*4) textSize ]
drawText "x"     = color black $
                   rotate 45 $
                   pictures [ rectangleSolid (textSize*6) textSize,
                              rectangleSolid textSize (textSize*6) ]
drawText "+"     = color black $
                   pictures [ rectangleSolid textSize (textSize*5),
                              rectangleSolid (textSize*5) textSize ]
drawText "-"     = color black $ rectangleSolid (textSize*5) textSize
drawText t@(c:cs) =
  if c == 'S' || c == 'R' || c == 'C'
    then color black $ translate (-spacing/4 + 2) (-spacing/12) $ scale 0.08 0.1 $ text t
    else pictures [ drawText [c], translate (spacing/4 - 2) (-spacing/12) $ scale 0.08 0.1 $ text cs ]

calcX :: Int -> Float
calcX c = spacing + spacing * (fromIntegral c)

calcY :: Int -> Float
calcY q = -(fromIntegral q) * spacing - spacing/2
