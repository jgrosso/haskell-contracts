module Tests
    (Test,
     TestSuite,
     runTest,
     runTests,
     Result(..)
    ) where

type Name = String
type Condition = Bool
type Test = (Name, Condition)

type TestSuite = [Test]

data Result = Success
            | Failure String

runTest :: Test -> Result
runTest (name, condition) = if condition then Success else Failure name

runTests :: TestSuite -> Result
runTests = foldl foldFn Success
    where foldFn acc x = case runTest x of
                           Success   -> acc
                           Failure y -> case acc of
                                          Success   -> Failure y
                                          Failure z -> Failure $ z ++ "; " ++ y
