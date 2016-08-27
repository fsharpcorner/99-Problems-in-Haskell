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

-------------------- Problem 05
tests_005 = test ["test005_01" ~: "(myReverse [1,2,3,4] )" ~: ([4,3,2,1]) ~=? (myReverse [1,2,3,4] )
                  , "test005_02" ~: "(myReverse ([]::[Int]))" ~: ([]) ~=? (myReverse ([]::[Int]))
                  , "test005_03" ~: "(myReverse ['z'])" ~: (['z']) ~=? (myReverse ['z'])
                  , "test005_04" ~: "(myReverse (A man, a plan, a canal, panama!)" ~: ("!amanap ,lanac a ,nalp a ,nam A") ~=? (myReverse "A man, a plan, a canal, panama!")]

-------------------- Problem 06
tests_006 = test ["test006_01" ~: "(isPalindrome [] )" ~: (False) ~=? (isPalindrome "")
                  , "test006_02" ~: "(isPalindrome [a])" ~: (True) ~=? (isPalindrome "a")
                  , "test006_03" ~: "(isPalindrome [abc])" ~: (False) ~=? (isPalindrome "abc")
                  , "test006_04" ~: "(isPalindrome [laval])" ~: (True) ~=? (isPalindrome "laval")]

-------------------- Problem 07
tests_007 = test ["test007_01" ~: "(flatten [] )" ~: ([]::[Char]) ~=? (flatten (List []))
                  ,"test007_02" ~: "(flatten ['a'])" ~: (['a']) ~=? (flatten (Elem 'a'))
                  , "test007_03" ~: "(flatten ['a',['b','c']])" ~: (['a','b','c']) ~=? (flatten (List [Elem 'a', List [Elem 'b',Elem 'c']]))]

main = runTestTT $ TestList [tests_001, tests_001', tests_001'', tests_002, tests_003, tests_003', tests_004, tests_005, tests_006, tests_007]
