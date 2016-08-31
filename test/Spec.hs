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

-------------------- Problem 08
tests_008 = test ["test008_01" ~: "(compress [] )" ~: ("") ~=? (compress "")
                  , "test008_02" ~: "(compress [abc])" ~: ("abc") ~=? (compress "abc")
                  , "test008_03" ~: "(compress [aaaabccaadeeee])" ~: ("abcade") ~=? (compress "aaaabccaadeeee")]

-------------------- Problem 09
tests_009 = test ["test009_01" ~: "(pack [] )" ~: ([]) ~=? (pack "")
                  , "test009_02" ~: "(pack [a])" ~: (["a"]) ~=? (pack "a")
                  , "test009_03" ~: "(pack [abc])" ~: (["a","b","c"]) ~=? (pack "abc")
                  , "test009_04" ~: "(pack [aaaabccaadeeee])" ~: (["aaaa","b","cc","aa","d","eeee"]) ~=? (pack "aaaabccaadeeee")]

-------------------- Problem 10
tests_010 = test ["test010_01" ~: "(encode [] )" ~: ([]) ~=? (encode "")
                  , "test010_02" ~: "(encode [a])" ~: ([(1,'a')]) ~=? (encode "a")
                  , "test010_03" ~: "(encode [abc])" ~: ([(1,'a'),(1,'b'),(1,'c')]) ~=? (encode "abc")
                  , "test010_04" ~: "(encode [aaaabccaadeeee])" ~: ([(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]) ~=? (encode "aaaabccaadeeee")]

-------------------- Problem 11
tests_011 = test ["test011_01" ~: "(encode_modified [] )" ~: ([]::[EncodeResult]) ~=? (encode_modified "")
                  , "test011_02" ~: "(encode_modified [a])" ~: ([Single 'a']) ~=? (encode_modified "a")
                  , "test011_03" ~: "(encode_modified [abc])" ~: ([Single 'a', Single 'b',Single 'c']) ~=? (encode_modified "abc")
                  , "test011_04" ~: "(encode_modified [aaaabccaadeeee])" ~: ([Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']) ~=? (encode_modified "aaaabccaadeeee")]

-------------------- Problem 12
tests_012 = test ["test012_01" ~: "(decode [] )" ~: ("") ~=? (decode [])
                  , "test012_02" ~: "(decode [a])" ~: ("a") ~=? (decode [Single 'a'])
                  , "test012_03" ~: "(decode [abc])" ~: ("abc") ~=? (decode [Single 'a', Single 'b',Single 'c'])
                  , "test012_04" ~: "(decode [aaaabccaadeeee])" ~: ("aaaabccaadeeee") ~=? (decode [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e'])]

-- -------------------- Problem 13
tests_013 = test ["test013_01" ~: "(encode_direct [] )" ~: ([]::[EncodeResult]) ~=? (encode_direct "")
                   , "test013_02" ~: "(encode_direct [a])" ~: ([Single 'a']) ~=? (encode_direct "a")
                   , "test013_03" ~: "(encode_direct [abc])" ~: ([Single 'a', Single 'b',Single 'c']) ~=? (encode_direct "abc")
                   , "test013_04" ~: "(encode_direct [aaaabccaadeeee])" ~: ([Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']) ~=? (encode_direct "aaaabccaadeeee")]

main = runTestTT $ TestList [tests_001, tests_001', tests_001'', tests_002, tests_003, tests_003', tests_004, tests_005, tests_006, tests_007, tests_008, tests_009, tests_010, tests_011, tests_012, tests_013] 
