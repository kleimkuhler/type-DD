module Exercises

import Functions

-- 2
||| Returns True if string is a palindrome, False otherwise.
||| @str the string to test if palindrome
palindrome : (str : String) -> Bool
palindrome str = str == reverse str

-- 3
||| Returns True if string is a case-insensitive palindrome, False otherwise.
||| @str the string to test if palindrome
nocasePalindrome : (str : String) -> Bool
nocasePalindrome str = let lowerStr = toLower str in
                           palindrome lowerStr

-- 4 & 5
||| Returns True if string is a palindrome and longer than minimum length,
||| False otherwise.
||| @len the length that string must be longer than
||| @str the string to test if palindrome
lengthPalindrome : (len : Nat) -> (str : String) -> Bool
lengthPalindrome len str
  = if length str > len then nocasePalindrome str else False

-- 6
||| Returns a tuple consisting of the number of words and number of characters
||| in a string respectively.
counts : String -> (Nat, Nat)
counts str = (length (words str), length str)

-- 7
||| Returns the largest ten items of a list.
top_ten : Ord a => List a -> List a
top_ten items = take 10 (reverse (sort items))

-- 8
||| Return the number of strings in a list longer than the given number of
||| characters.
over_length : Nat -> List String -> Nat
over_length len items = length (filter (> len) (map length items))
