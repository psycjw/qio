module Main where

import Render
import Circuit
import Logic
import Graphics.Gloss
import Graphics.Gloss.Data.Color

window :: Display
window = InWindow "Circuit" (floor width, floor height) (10, 10)

main :: IO ()
main = play window              -- window to show             :: Display
            white               -- background color           :: Color
            30                  -- fps                        :: Int
            initialCircuit      -- initial world              :: Circuit
            draw                -- function to draw world     :: (Circuit -> Picture)
            handleEvent         -- function to handle inputs  :: (Event -> Circuit -> Circuit)
            (const id)          -- function to step world     :: (Float -> Circuit -> Circuit)
