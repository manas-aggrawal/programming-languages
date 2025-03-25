# Source: https://en.wikipedia.org/wiki/Eight_queens_puzzle
import sys

def queens(n: int, i: int, a: list, b: list, c: list):
    if i < n:
        for j in range(n):
            if j not in a and i + j not in b and i - j not in c:
                yield from queens(n, i + 1, a + [j], b + [i + j], c + [i - j])
    else:
        yield a

def nqueens(n, all=0):
    if all:
        for i in range(all):
            print(next(queens(n, 0, [], [], [])))
            return

    for solution in queens(n, 0, [], [], []):
        print(solution)

if __name__ == "__main__":
    if not (len(sys.argv) == 2 or len(sys.argv) == 3):
        print("Usage: python_solution.py <size of board: int> <num sols: optional int>")
        exit(1)
    n = int(sys.argv[1])
    if len(sys.argv) == 3:
        nqueens(n, int(sys.argv[2]))
    else:
        nqueens(n)

