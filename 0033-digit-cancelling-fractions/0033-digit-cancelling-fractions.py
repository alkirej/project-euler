

def removeSimilarDigits( num, den ):
    (numten,numone) = ( num // 10, num % 10)
    (denten,denone) = ( den // 10, den % 10)

    if numten==denone:
        return (numone,denten)
    elif numone==denten:
        return (numten,denone)
    else:
        return (0,0)

for numerator in range(10,100):
    for denominator in range(10,100):
        if denominator > numerator:
            (newNum,newDen) = removeSimilarDigits(numerator, denominator)
            if newNum>0 and newDen>0:
                if newNum * denominator == newDen * numerator:
                    print( str(numerator) + "/" + str(denominator), end=" ")
                    print( str(newNum) + "/" + str(newDen))

