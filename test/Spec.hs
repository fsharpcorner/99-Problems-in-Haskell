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
tests_001' = test ["test001_01" ~: "(myLast' [1,2,3,4])" ~: (Just 4) ~=? (myLast' [1,2,3,4])
                  , "test001_02" ~: "(myLast' [])" ~: Nothing ~=? (myLast' ([]::[Int]))
                  , "test001_03" ~: "(myLast' [1])" ~: (Just 1) ~=? (myLast' [1])
                  , "test001_04" ~: "(myLast' ['a','b','c','d'])" ~: (Just 'd') ~=?(myLast' ['a','b','c','d'])]
tests_001'' = test ["test001_01" ~: "(myLast'' [1,2,3,4])" ~: (Just 4) ~=? (myLast'' [1,2,3,4])
                  , "test001_02" ~: "(myLast'' [])" ~: Nothing ~=? (myLast'' ([]::[Int]))
                  , "test001_03" ~: "(myLast'' [1])" ~: (Just 1) ~=? (myLast'' [1])
                  , "test001_04" ~: "(myLast'' ['a','b','c','d'])" ~: (Just 'd') ~=?(myLast'' ['a','b','c','d'])]

--test01_1 = TestCase (assertEqual "for (myLast [1,2,3,4])," (Just 4) (testedFunction01 [1,2,3,4]))



main = runTestTT $ TestList [tests_001, tests_001', tests_001'']
