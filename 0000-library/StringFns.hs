module StringFns
    ( isPalindrome
    )
where


isPalindrome :: String -> Bool
isPalindrome str = str == reverse str