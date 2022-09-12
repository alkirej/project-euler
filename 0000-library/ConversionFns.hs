module ConversionFns
    (   numberToWord
    )
where

import qualified Data.Text                      as Txt

numberToWord :: Int -> Txt.Text
numberToWord n
    | n  <    1  = error "Invalid value ("
                        `Txt.append` Txt.pack (show n)
                        `Txt.append` ") in numberToWord."
                        `Txt.append` "(Only positive numbers allowed.)"
    | n  <   10  = onesDigit n
    | n  <  100  = twoDigitNumber n
    | n  < 1000  = threeDigitNumber n
    | n == 1000  = "one thousand"
    | otherwise  = error "Invalid value ("
                          `Txt.append` Txt.pack (show n)
                          `Txt.append` ") in numberToWord."
                          `Txt.append` "(Only implementd to 1,000.)"

twoDigitNumber :: Int -> Txt.Text
twoDigitNumber 10 = "ten"
twoDigitNumber 11 = "eleven"
twoDigitNumber 12 = "twelve"
twoDigitNumber 13 = "thirteen"
twoDigitNumber 14 = "fourteen"
twoDigitNumber 15 = "fifteen"
twoDigitNumber 18 = "eighteen"
twoDigitNumber n
    | n  <  10    = onesDigit n
    | n  <  20    = onesDigit (n `mod` 10) `Txt.append` "teen"
    | n  < 100    =
        let tens    = tensDigit (n `div` 10)
            ones    = "-" `Txt.append` onesDigit (n `mod` 10)
            baseTen = (n `mod` 10)  ==  0
        in tens `Txt.append` if baseTen then "" else ones
    | otherwise   = error "Invalid value (" `Txt.append` Txt.pack (show n)
                        `Txt.append` ") in twoDigitNumber"

threeDigitNumber :: Int -> Txt.Text
threeDigitNumber n
    | n < 100 || n > 1000 = error "Invalid value ("
                                    `Txt.append` Txt.pack (show n)
                                    `Txt.append` ") in threeDigitNumber"
threeDigitNumber n =
    let ndAnd  =  n `mod` 100 /= 0
        hdTxt  =  hundredsDigit $ n `div` 100
    in  if ndAnd then
            hdTxt `Txt.append` " and "
                  `Txt.append` twoDigitNumber (n `mod` 100)
        else
            hdTxt

onesDigit :: Int -> Txt.Text
onesDigit 0 = ""
onesDigit 1 = "one"
onesDigit 2 = "two"
onesDigit 3 = "three"
onesDigit 4 = "four"
onesDigit 5 = "five"
onesDigit 6 = "six"
onesDigit 7 = "seven"
onesDigit 8 = "eight"
onesDigit 9 = "nine"
onesDigit n = error "Invalid value (" `Txt.append` Txt.pack (show n)
                        `Txt.append` ") in onesDigit."

tensDigit :: Int -> Txt.Text
tensDigit 0 = ""
tensDigit 1 = ""
tensDigit 2 = "twenty"
tensDigit 3 = "thirty"
tensDigit 4 = "forty"
tensDigit 5 = "fifty"
tensDigit 6 = "sixty"
tensDigit 7 = "seventy"
tensDigit 8 = "eighty"
tensDigit 9 = "ninety"
tensDigit n = error "Invalid value (" `Txt.append` Txt.pack (show n)
                        `Txt.append` ") in tensDigit."

hundredsDigit :: Int -> Txt.Text
hundredsDigit n = onesDigit n `Txt.append` " hundred"
