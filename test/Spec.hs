module Main (main) where

import SolveQuad
import Test.HUnit
import Data.List (sort)

(~=) :: Double -> Double -> Bool
x ~= y = abs (x - y) < 0.000001

main :: IO Counts
main = runTestTT allTests

testTwoRealRoots :: Test
testTwoRealRoots = TestCase $
    let
        eq = Equation { a = 1.0, b = -5.0, c = 6.0 }
        expectedRoots = [2.0, 3.0]
        actual = solve eq
    in
    case actual of
        Two r1 r2 ->
            let actualRoots = sort [r1, r2]
            in assertBool "Roots do not match" $
               (actualRoots !! 0) ~= (expectedRoots !! 0) &&
               (actualRoots !! 1) ~= (expectedRoots !! 1)
        _ -> assertFailure "Expected two roots (Two), got something else"

testOneRealRoot :: Test
testOneRealRoot = TestCase $
    let
        eq = Equation { a = 1.0, b = -4.0, c = 4.0 }
        actual = solve eq
    in
    case actual of
        One r -> assertBool "Root is not equal to 2.0" (r ~= 2.0)
        _ -> assertFailure "Expected one root (One), got something else"

testComplexRoots :: Test
testComplexRoots = TestCase $
    let
        eq = Equation { a = 1.0, b = 0.0, c = 4.0 }
        actual = solve eq
    in
    case actual of
        Complex real imag ->
            assertBool "Complex roots are incorrect: expected 0.0 ± 2.0i" $
                (real ~= 0.0) && (imag ~= 2.0)
        _ -> assertFailure "Expected complex roots (Complex), got something else"


testLinearCase :: Test
testLinearCase = TestCase $
    let
        eq = Equation { a = 0.0, b = 2.0, c = -10.0 }
        actual = solve eq
    in
    case actual of
        One r -> assertBool "Root is not equal to 5.0" (r ~= 5.0)
        _ -> assertFailure "Expected one root (One), got something else"

testNoSolution :: Test
testNoSolution = TestCase $
    let
        eq = Equation { a = 0.0, b = 0.0, c = 5.0 }
        actual = solve eq
    in
    case actual of
        SpecialCase s -> assertEqual "Expected SpecialCase 'No solution'" "No solution (e.g., 5=0)" s
        _ -> assertFailure "Expected SpecialCase, got something else"

testInfiniteSolution :: Test
testInfiniteSolution = TestCase $
    let
        eq = Equation { a = 0.0, b = 0.0, c = 0.0 }
        actual = solve eq
    in
    case actual of
        SpecialCase s -> assertEqual "Expected SpecialCase 'Infinite solutions'" "Infinite solutions (0=0)" s
        _ -> assertFailure "Expected SpecialCase, got something else"


allTests :: Test
allTests = TestList
    [ "Two roots"         ~: testTwoRealRoots
    , "One root"          ~: testOneRealRoot
    , "Complex roots"     ~: testComplexRoots
    , "Linear"            ~: testLinearCase
    , "No solutions"      ~: testNoSolution
    , "Infinite solutions"~: testInfiniteSolution
    ]


