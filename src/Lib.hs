module Lib

( someFunc
, myLast
, myLast'
, myLast''
, myButLast
, elementAt
, elementAt'
, myLength
, myReverse
, isPalindrome
, flatten
, NestedList(..)
) where

-------------------- Problem 01

myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast [x] = Just x
myLast l = Just(head (reverse l))

myLast' :: [a] -> Maybe a
myLast' [] = Nothing
myLast' [x] = Just x
myLast' l = Just(last l)

myLast'' :: [a] -> Maybe a
myLast'' [] = Nothing
myLast'' [x] = Just x
myLast'' (h:t)  = myLast'' t

-------------------- Problem 02

myButLast :: [a] -> Maybe a
myButLast [] = Nothing
myButLast [x] = Nothing
myButLast (x:y:[]) = Just x
myButLast (x:y:xs) = myButLast (y:xs)


-------------------- Problem 03
elementAt :: [a] -> Int -> Maybe a
elementAt l n
 | n < 1 = Nothing
 | n > (length l) = Nothing
 | otherwise = Just (l !! (n-1))


elementAt' :: [a] -> Int -> Maybe a
elementAt' [] n = Nothing
elementAt' (x:xs) n
 | n < 1 = Nothing
 | n > (length (x:xs)) = Nothing
 | n == 1 = Just x
 | otherwise = elementAt' xs (n-1)

-------------------- Problem 04

myLength :: [a] -> Int
myLength l = myLengthImpl l 0
myLengthImpl [] n = n
myLengthImpl [x] n = n+1
myLengthImpl (x:xs) n = myLengthImpl xs (n+1)

-------------------- Problem 05
myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse l = myReverseImpl l []
myReverseImpl :: [a] -> [a] -> [a]
myReverseImpl [] nl = nl
myReverseImpl (x:xs) nl = myReverseImpl xs (x:nl)

-------------------- Problem 06

isPalindrome :: [Char] -> Bool
isPalindrome [] = False
isPalindrome w = w == (reverse w)

-------------------- Problem 07
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []


someFunc :: IO ()
someFunc =
	putStrLn "someFunc"
