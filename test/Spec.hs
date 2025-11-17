{-# OPTIONS -Wno-incomplete-uni-patterns #-}
{-# OPTIONS -Wno-missing-signatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.List (sort)
import Test.HUnit.Approx (assertApproxEqual)
import Test.Tasty.HUnit
import Test.Tasty.TH

import SolveQuad

main = $defaultMainGenerator

x @?~ y = assertApproxEqual "" epsilon y x where epsilon = 0.000001
infix 1 @?~

case_twoRealRoots =
    case solve Equation{a = 1, b = -5, c = 6} of
        Two r1 r2 -> do
            actualRoot1 @?~ expectedRoot1
            actualRoot2 @?~ expectedRoot2
          where
            [actualRoot1, actualRoot2] = sort [r1, r2]
        _ -> assertFailure "Expected two roots (Two), got something else"
  where
    expectedRoot1 = 2
    expectedRoot2 = 3

case_oneRealRoot = do
    case solve Equation{a = 1, b = -4, c = 4} of
        One r -> r @?~ 2
        _ -> assertFailure "Expected one root (One), got something else"

case_complexRoots =
    case solve Equation{a = 1, b = 0, c = 4} of
        Complex real imag -> do
            real @?~ 0
            imag @?~ 2
        _ ->
            assertFailure "Expected complex roots (Complex), got something else"

case_linearCase =
    case solve Equation{a = 0, b = 2, c = -10} of
        One r -> r @?~ 5
        _ -> assertFailure "Expected one root (One), got something else"

case_noSolution = solve Equation{a = 0, b = 0, c = 5} @?= NoSolution

case_infiniteSolution =
    solve Equation{a = 0, b = 0, c = 0} @?= InfiniteSolutions
