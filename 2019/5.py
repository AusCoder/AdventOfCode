from collections import deque
from pathlib import Path

from intcode import run_intcode, IntcodeState


def part1():
    instructions = [int(x) for x in Path("input/5.txt").read_text().strip().split(",")]
    inpts = deque([1])
    outputs = deque()
    run_intcode(IntcodeState(instructions), inpts, outputs)
    print(outputs)


def part2():
    instructions = [int(x) for x in Path("input/5.txt").read_text().strip().split(",")]
    inpts = deque([5])
    outputs = deque()
    run_intcode(IntcodeState(instructions), inpts, outputs)
    print(outputs)


if __name__ == "__main__":
    part1()
    part2()
