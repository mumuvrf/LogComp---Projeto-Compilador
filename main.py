import sys

def solve_equation(equation: str):
    symbols = ['+', '-']
    result = 0
    number = ""
    sum = 1
    sub = 0
    for char in equation:
        if char in symbols:
            if sum:
                result += int(number)
                sum = 0
            elif sub:
                result -= int(number)
                sub = 0

            if char == '+':
                sum = 1
            elif char == '-':
                sub = 1

            number = ""
        else:
            number += char

    if sum:
        result += int(number)
        sum = 0
    elif sub:
        result -= int(number)
        sub = 0

    return result

if __name__ == '__main__':
    equation = sys.argv[1]
    try:
        solution = solve_equation(equation)
        print(solution)
    except:
        raise Exception("Not a valid equation.")