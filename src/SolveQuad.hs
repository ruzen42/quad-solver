{-# LANGUAGE RecordWildCards #-}

module SolveQuad
    ( solve
    , Equation(..)
    , EquationSolution(..)
    ) where

data Equation = Equation
    { a :: Double
    , b :: Double
    , c :: Double
    } deriving (Show)

data EquationSolution
    = One Double
    | Two Double Double
    | Complex Double Double
    | SpecialCase String
    deriving (Show)


getDiscriminant :: Double -> Double -> Double -> Double
getDiscriminant a b c = b*b - 4*a*c

solve :: Equation -> EquationSolution
solve Equation{..}
    | a == 0.0 =
        if b == 0.0
            then if c == 0.0
                then SpecialCase "Infinite solutions (0=0)"
                else SpecialCase "No solution (e.g., 5=0)"
            else
                One (-c / b)

    | otherwise =
        let
            d = getDiscriminant a b c
            x_real_part = -b / (2*a)
        in
        case compare d 0 of
            GT ->
                let
                    d_sqrt = sqrt d
                    x1 = x_real_part + d_sqrt / (2*a)
                    x2 = x_real_part - d_sqrt / (2*a)
                in
                Two x1 x2

            EQ -> One x_real_part

            LT ->
                let
                    d_imag_part = sqrt (-d) / (2*a)
                in
                Complex x_real_part d_imag_part

