import itertools
from pathlib import Path

inpt_op_codes = [int(x) for x in Path("input/2.txt").read_text().strip().split(",")]


def run(op_codes):
    p = 0
    while op_codes[p] != 99:
        arg1 = op_codes[op_codes[p + 1]]
        arg2 = op_codes[op_codes[p + 2]]
        idx_res = op_codes[p + 3]
        if op_codes[p] == 1:
            op_codes[idx_res] = arg1 + arg2
        elif op_codes[p] == 2:
            op_codes[idx_res] = arg1 * arg2
        else:
            raise RuntimeError
        p += 4


def part1():
    op_codes = list(inpt_op_codes)
    op_codes[1] = 12
    op_codes[2] = 2
    run(op_codes)
    print(op_codes[0])


def part2():
    for n, v in itertools.product(range(100), range(100)):
        op_codes = list(inpt_op_codes)
        op_codes[1] = n
        op_codes[2] = v
        run(op_codes)
        if op_codes[0] == 19690720:
            print(100 * n + v)
            break


part2()
