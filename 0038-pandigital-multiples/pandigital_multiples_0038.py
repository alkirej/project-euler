"""
Author: Jeff Alkire
"""
#import sys
#sys.path.append("/home/jeff/git/euler-project/0000-library/Python")

import cmd_line as cl

from itertools import permutations

def main():
    """ Main fn. """
    print()
    print()
    print("Working on euler problem #0038.")
    print()

    up_to = cl.get_numeric_command_line_argument()

    for n in range(up_to):
        to_verify = check_mults(n)
        if is_pandigital(to_verify):
            print(to_verify)

def check_mults(num):
    results=""
    current_n=0
    while len(results) < 9:
        current_n += 1
        product = current_n * num
        results += str(product)
    return results

def is_pandigital(str):
    new_str = "".join(sorted(str))
    return new_str == "123456789"


def digits_to_num( nums, start, stop):
    returnVal = 0
    for n in range (start,stop+1):
        returnVal = returnVal*10 + nums[n]

    return returnVal

def correct_math( nums ):
    op1  = digits_to_num(nums, 0, 0)
    op2  = digits_to_num(nums, 1, 4)
    prod = digits_to_num(nums, 5, 8)
    if op1 * op2 == prod:
        return True

    op1  = digits_to_num(nums, 0, 1)
    op2  = digits_to_num(nums, 2, 4)
    prod = digits_to_num(nums, 5, 8)

    return op1 * op2 == prod

if __name__ == "__main__":
    main()

