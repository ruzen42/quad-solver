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
            in assertBool "Корни не совпадают" $
               (actualRoots !! 0) ~= (expectedRoots !! 0) &&
               (actualRoots !! 1) ~= (expectedRoots !! 1)
        _ -> assertFailure "Ожидалось два корня (Two), получено другое"

testOneRealRoot :: Test
testOneRealRoot = TestCase $
    let
        eq = Equation { a = 1.0, b = -4.0, c = 4.0 }
        actual = solve eq
    in
    case actual of
        One r -> assertBool "Корень не равен 2.0" (r ~= 2.0)
        _ -> assertFailure "Ожидался один корень (One), получено другое"

testComplexRoots :: Test
testComplexRoots = TestCase $
    let
        eq = Equation { a = 1.0, b = 0.0, c = 4.0 }
        actual = solve eq
    in
    case actual of
        Complex real imag ->
            assertBool "Комплексные корни неверны: ожидалось 0.0 ± 2.0i" $
                (real ~= 0.0) && (imag ~= 2.0)
        _ -> assertFailure "Ожидались комплексные корни (Complex), получено другое"


testLinearCase :: Test
testLinearCase = TestCase $
    let
        eq = Equation { a = 0.0, b = 2.0, c = -10.0 }
        actual = solve eq
    in
    case actual of
        One r -> assertBool "Корень не равен 5.0" (r ~= 5.0)
        _ -> assertFailure "Ожидался один корень (One), получено другое"

testNoSolution :: Test
testNoSolution = TestCase $
    let
        eq = Equation { a = 0.0, b = 0.0, c = 5.0 }
        actual = solve eq
    in
    case actual of
        SpecialCase s -> assertEqual "Ожидался SpecialCase 'No solution'" "No solution (e.g., 5=0)" s
        _ -> assertFailure "Ожидался SpecialCase, получено другое"

testInfiniteSolution :: Test
testInfiniteSolution = TestCase $
    let
        eq = Equation { a = 0.0, b = 0.0, c = 0.0 }
        actual = solve eq
    in
    case actual of
        SpecialCase s -> assertEqual "Ожидался SpecialCase 'Infinite solutions'" "Infinite solutions (0=0)" s
        _ -> assertFailure "Ожидался SpecialCase, получено другое"


allTests :: Test
allTests = TestList
    [ "Two roots"         ~: testTwoRealRoots
    , "One root"          ~: testOneRealRoot
    , "Complex roots"     ~: testComplexRoots
    , "Linear"            ~: testLinearCase
    , "No solutions"      ~: testNoSolution
    , "Infinity solution" ~: testInfiniteSolution
    ]

