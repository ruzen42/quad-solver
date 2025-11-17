module Main (main) where

import SolveQuad (solve)

main :: IO ()
main = solve Equation {a = 1, b = 2, c = 3}
