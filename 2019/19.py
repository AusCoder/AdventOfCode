import itertools
from collections import deque
from pathlib import Path

from intcode import IntcodeState, InputRequiredError, run_intcode


def read_input():
    return [int(x) for x in Path("input/19.txt").read_text().strip().split(",")]


class State:
    def __init__(self, instructions):
        self.instructions = instructions

    def tractor_beam_size(self):
        grid = [[" " for _ in range(50)] for _ in range(50)]
        beam_posns = set()

        for x, y in itertools.product(range(50), range(50)):
            state = IntcodeState(self.instructions)
            inputs = deque([x, y])
            outputs = deque()
            run_intcode(state, inputs, outputs)
            (output,) = outputs

            if output == 1:
                beam_posns.add((x, y))
            grid[y][x] = "#" if output == 1 else "."

        lines = ["".join(r) for r in grid]
        lines = [f"{i:2d} {l}" for i, l in enumerate(lines)]
        fst_line = "   " + "".join(f"{x % 10:1d}" for x in range(len(grid[0])))

        print("\n".join([fst_line, *lines]))
        print(len(beam_posns))

        for y_pos in range(36):
            assert sum(1 for x, y in beam_posns if y == y_pos) == row_length(y_pos)
            try:
                assert min(x for x, y in beam_posns if y == y_pos) == row_start(y_pos)
            except ValueError:
                pass

    def check(self, x, y, size):
        pts = [
            (x, y),
            (x + size - 1, y),
            (x, y + size - 1),
            (x + size - 1, y + size - 1),
        ]
        for x, y in pts:
            state = IntcodeState(self.instructions)
            inputs = deque([x, y])
            outputs = deque()
            run_intcode(state, inputs, outputs)
            (output,) = outputs
            assert output == 1


def part1():
    s = State(read_input())
    s.tractor_beam_size()


def row_length(n):
    if n == 0:
        return 1
    n -= 1
    return (n // 12) * 3 + ((n % 12 + 1) // 3)


def row_start(n):
    if n == 0:
        return 0
    if n == 1 or n == 2:
        return None
    return (n - 1) // 12 + n + 1


def part2():
    size = 100
    i = 3
    while True:
        left = row_start(i + size - 1)
        det = row_start(i) + row_length(i) - left
        if det >= size:
            break
        i += 1
    print(left, i)
    print(10000 * left + i)

    s = State(read_input())
    s.check(left, i, size)


if __name__ == "__main__":
    part1()
    part2()
