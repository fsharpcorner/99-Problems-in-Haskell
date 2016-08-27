module Lib

( someFunc
, foo
, myLast
, myLast'
, myLast''
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



someFunc :: IO ()
someFunc =
	putStrLn "someFunc"
