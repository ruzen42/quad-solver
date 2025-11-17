{-# LANGUAGE RecordWildcard #-}

module Lib
    ( solve
    ) where

data Rules
    = First
    | Second
    | Third
    | Fouth
    deriving (Enum, Show)

data EquationType
    = Rule Rules
    | Discriminant
    | Viet
    deriving (Enum, Show)

data Equation = Equation
    { type :: EquationType
    , a    :: Integer
    , b    :: Integer
    , c    :: Integer
    } deriving (Show)

data EquationSolution
    = One Integer
    | Two Integer Integer
    | Complex Integer Integer

solve :: Equation -> EquationSolution
solve Equation{..} =
    case type of
        Rule rule -> case rule of
                       First  -> solveDiscriminant a b c
                       Second -> solveDiscriminant a b c
                       Third  -> solveDiscriminant a b c
                       Fouth  -> solveDiscriminant a b c
        Discriminant -> solveDiscriminant a b c
        Viet -> solveDiscriminant a b c

getDiscriminant :: Integer -> Integer -> Integer ->

solveDiscriminant :: Integer -> Integer -> Integer -> EquationSolution
solveDiscriminant =
