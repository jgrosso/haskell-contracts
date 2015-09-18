module Example where

import Tests(runTests, Result(..))
import Contracts(contractsTests)

main :: IO ()
main = putStrLn $ case runTests contractsTests of
                    Success -> "Success"
                    Failure x -> "Failure: " ++ x
