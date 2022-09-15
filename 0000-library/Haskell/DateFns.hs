module DateFns
    (   DayOfWeek(..),
        SimpleDate(..),
        addSevenDays,
        dayOfWeek,
        daysUntilSunday
    )
where

type SimpleDate = (Int,Int,Int)

data DayOfWeek = Sunday
                 | Monday
                 | Tuesday
                 | Wednesday
                 | Thursday
                 | Friday
                 | Saturday
    deriving Enum

knownDate :: (SimpleDate, DayOfWeek)
knownDate = ((1900,1,1), Monday)

dayOfWeek :: SimpleDate -> DayOfWeek
dayOfWeek dt
    | dt == (1900,01,01) = Monday
    | dt == (1901,01,01) = Tuesday
    | otherwise = error "Routine not fully implemented at this time."

daysUntilSunday :: DayOfWeek -> Int
daysUntilSunday dow = 7 - fromEnum dow

addSevenDays :: SimpleDate -> SimpleDate
addSevenDays (yr,mo,dy) =
    let simple = (yr,mo,dy+7)
        nextVal = if dy+7 >28 then newMonthCheck simple else simple
    in  nextVal

newMonthCheck :: SimpleDate -> SimpleDate
newMonthCheck orig@(yr,mo,dy) =
    let dim = daysInMonth yr mo
    in  if dy > dim then
            let (nxtMo,nxtYr) = if mo == 12 then
                                  (1,yr+1)
                                else
                                  (mo+1,yr)
                nxtDy = dy - dim
            in  (nxtYr,nxtMo,nxtDy)
        else
            orig



daysInMonth :: Int -> Int -> Int
daysInMonth yr  1 = 31
daysInMonth yr  2 = 28 + if isLeapYear yr then 1 else 0
daysInMonth yr  3 = 31
daysInMonth yr  4 = 30
daysInMonth yr  5 = 31
daysInMonth yr  6 = 30
daysInMonth yr  7 = 31
daysInMonth yr  8 = 31
daysInMonth yr  9 = 30
daysInMonth yr 10 = 31
daysInMonth yr 11 = 30
daysInMonth yr 12 = 31

isLeapYear :: Int -> Bool
isLeapYear year
    | 0 == year `mod` 400 = True
    | 0 == year `mod` 100 = False
    | 0 == year `mod`   4 = True
    | otherwise      = False