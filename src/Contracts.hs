module Contracts
    (Precondition,
     Postcondition,
     contract,
     contractsTests
    ) where

import Tests (runTests, Test, Result(..), TestSuite)

type Name = String

type Condition     a = (Name, a, a -> Bool)
type Precondition  a = Condition a
type Postcondition a = Condition a

contract :: [Precondition a] -> (a -> b) -> a -> [Postcondition b] -> b
contract preconditions function argument postconditions = case runTests $ map conditionToTest preconditions of
                                                            Success   -> case runTests $ map conditionToTest postconditions of
                                                                           Success   -> function argument
                                                                           Failure x -> error x
                                                            Failure x -> error x
    where conditionToTest (name, conditionArgument, condition) = (name, condition conditionArgument)

contractsTest1 :: Test
contractsTest1 = ("Test 1", (3 :: Int) == contract [("Precondition 1", arg, (> 1))] fn arg [("Postcondition 1", result, (> 1))])
    where arg = 3
          fn = (+ 1)
          result = fn arg

contractsTests :: TestSuite
contractsTests = [contractsTest1]