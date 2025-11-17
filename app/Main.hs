module Main (main) where

import SolveQuad
import Text.Read (readMaybe)

main :: IO ()
main = do
    input <- getLine
    let eq = createEquation input
    case eq of
        Just a -> print $ solve a
        Nothing -> error "Error while parsing"

parseThreeNumbers :: String -> Maybe (Double, Double, Double)
parseThreeNumbers input =
    case map readMaybe (words input) of
        [Just n1, Just n2, Just n3] -> Just (n1, n2, n3)
        _ -> Nothing

createEquation :: String -> Maybe Equation
createEquation input = do
    (f, s, t) <- parseThreeNumbers input
    return Equation { a = f, b = s, c = t }
