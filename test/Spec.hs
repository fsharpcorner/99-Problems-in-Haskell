--module MyTests where
module Main
where

import Test.HUnit
import Lib


-------------------- Problem 01

tests_001 = test ["test001_01" ~: "(myLast [1,2,3,4])" ~: (Just 4) ~=? (myLast [1,2,3,4])
                  , "test001_02" ~: "(myLast [])" ~: Nothing ~=? (myLast ([]::[Int]))
                  , "test001_03" ~: "(myLast [1])" ~: (Just 1) ~=? (myLast [1])
                  , "test001_04" ~: "(myLast ['a','b','c','d'])" ~: (Just 'd') ~=?(myLast ['a','b','c','d'])]
tests_001' = test ["test001_01'" ~: "(myLast' [1,2,3,4])" ~: (Just 4) ~=? (myLast' [1,2,3,4])
                  , "test001_02'" ~: "(myLast' [])" ~: Nothing ~=? (myLast' ([]::[Int]))
                  , "test001_03'" ~: "(myLast' [1])" ~: (Just 1) ~=? (myLast' [1])
                  , "test001_04'" ~: "(myLast' ['a','b','c','d'])" ~: (Just 'd') ~=?(myLast' ['a','b','c','d'])]
tests_001'' = test ["test001_01'" ~: "(myLast'' [1,2,3,4])" ~: (Just 4) ~=? (myLast'' [1,2,3,4])
                  , "test001_02'" ~: "(myLast'' [])" ~: Nothing ~=? (myLast'' ([]::[Int]))
                  , "test001_03'" ~: "(myLast'' [1])" ~: (Just 1) ~=? (myLast'' [1])
                  , "test001_04'" ~: "(myLast'' ['a','b','c','d'])" ~: (Just 'd') ~=?(myLast'' ['a','b','c','d'])]

--test01_1 = TestCase (assertEqual "for (myLast [1,2,3,4])," (Just 4) (testedFunction01 [1,2,3,4]))

-------------------- Problem 02

tests_002 = test ["test002_01" ~: "(myButLast [1,2,3,4])" ~: (Just 3) ~=? (myButLast [1,2,3,4])
                  , "test002_02" ~: "(myButLast [])" ~: Nothing ~=? (myButLast ([]::[Int]))
                  , "test002_03" ~: "(myButLast [1])" ~: Nothing ~=? (myButLast [1])
                  , "test002_04" ~: "(myButLast ['a','b','c','d'])" ~: (Just 'c') ~=?(myButLast ['a','b','c','d'])]

-------------------- Problem 03

tests_003 = test ["test003_01" ~: "(elementAt [1,2,3,4] 2 )" ~: (Just 2) ~=? (elementAt [1,2,3,4] 2 )
                  , "test003_02" ~: "(elementAt ([]::[Int]) 1)" ~: Nothing ~=? (elementAt ([]::[Int]) 1)
                  , "test003_03" ~: "(elementAt [1,2,3,4] 5 )" ~: Nothing ~=? (elementAt [1,2,3,4] 5 )
                  , "test003_04" ~: "(elementAt ['a','b','c','d'] 3)" ~: (Just 'c') ~=? (elementAt ['a','b','c','d'] 3)]


tests_003' = test ["test003_01'" ~: "(elementAt [1,2,3,4] 2 )" ~: (Just 2) ~=? (elementAt' [1,2,3,4] 2 )
                  , "test003_02'" ~: "(elementAt ([]::[Int]) 1)" ~: Nothing ~=? (elementAt' ([]::[Int]) 1)
                  , "test003_03'" ~: "(elementAt [1,2,3,4] 5 )" ~: Nothing ~=? (elementAt' [1,2,3,4] 5 )
                  , "test003_04'" ~: "(elementAt ['a','b','c','d'] 3)" ~: (Just 'c') ~=? (elementAt' ['a','b','c','d'] 3)]

-------------------- Problem 04

tests_004 = test ["test004_01" ~: "(myLength [1,2,3,4] )" ~: (4) ~=? (myLength [1,2,3,4] )
                  , "test004_02" ~: "(myLength ([]::[Int]))" ~: (0) ~=? (myLength ([]::[Int]))
                  , "test004_03" ~: "(myLength ['z'])" ~: (1) ~=? (myLength ['z'])]

main = runTestTT $ TestList [tests_001, tests_001', tests_001'', tests_002, tests_003, tests_003', tests_004]
