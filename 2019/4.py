# This really seems like a dynamic programming problem to me
# but I am too out of practice to figure out how to do it right now

import itertools

MIN = 353096
MAX = 843212


def increasing_with_double(n):
    n = str(n)
    return any(x == y for x, y in zip(n, n[1:])) and all(
        x <= y for x, y in zip(n, n[1:])
    )


def part1():
    print(sum(1 for x in range(MIN, MAX + 1) if increasing_with_double(x)))


def increasing_with_double_not_in_group(n):
    n = str(n)
    return any(len(list(g)) == 2 for _, g in itertools.groupby(n)) and all(
        x <= y for x, y in zip(n, n[1:])
    )


def part2():
    print(sum(1 for x in range(MIN, MAX + 1) if increasing_with_double_not_in_group(x)))


# part1()
part2()
