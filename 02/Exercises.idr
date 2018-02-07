module Exercises

import Functions

-- 1
||| Check if a string is a case-sensitive palindrome
||| @str a string to check if case-sensitive palindrome
casePalindrome : (str : String) -> Bool
casePalindrome str = str == reverse str

||| Check if a string is a case-insensitive palindrome
||| @str a string to check if case-insensitive palindrome
palindrome : (str : String) -> Bool
palindrome str = let lowerStr = toLower str in
                     casePalindrome lowerStr
