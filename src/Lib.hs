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
, compress
, pack
, encode
, encode_modified
, EncodeResult(..)
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

-------------------- Problem 08
compress :: [Char] -> [Char]
compress [] = []
compress l = compressImpl l []
compressImpl l nl
 | length (l) == 0 = reverse (nl)
 | length (nl) == 0 = compressImpl (tail l) ((head l):nl)
 | (head l) == (head nl) = compressImpl (tail l) nl
 | otherwise = compressImpl (tail l) ((head l):nl)

 -------------------- Problem 09
pack :: [Char] -> [[Char]]
pack [] = []
pack l = packImpl l [] []
packImpl l nl current
 | length (l) == 0 && length (current) == 0 = reverse (nl)
 | length (l) == 0 = reverse (current:nl)
 | length (current) == 0 = packImpl (tail l) nl ((head l):current)
 | (head l) == (head current) = packImpl (tail l) nl ((head l):current)
 | otherwise = packImpl (tail l) (current:nl) ((head l):[])

-------------------- Problem 10
encode :: [Char] -> [(Int, Char)]
encode [] = []
encode l = reverse(encodeImpl (pack l))
encodeImpl = foldl (\acc x -> ((length x),(head x)) : acc) []

-------------------- Problem 11
data EncodeResult = Single Char | Multiple Int Char
	deriving (Eq, Show)

parse_result :: (Int, Char) -> EncodeResult
parse_result r
 | fst(r) ==1 = Single (snd r)
 | otherwise = Multiple (fst r ) (snd r)

encode_modified :: [Char] -> [EncodeResult]
encode_modified [] = []
encode_modified l = reverse(encode_modifiedImpl (pack l))
encode_modifiedImpl = foldl (\acc x -> (parse_result ((length x),(head x)) ) : acc) []

someFunc :: IO ()
someFunc =
	putStrLn "someFunc"
