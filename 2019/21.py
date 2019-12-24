from collections import deque
from pathlib import Path

from intcode import IntcodeState, run_intcode, InputRequiredError


def read_input():
    return [int(x) for x in Path("input/21.txt").read_text().strip().split(",")]


def gen_char_outputs(outputs):
    for c in outputs:
        try:
            yield chr(c)
        except ValueError:
            yield str(c)


def part1():
    state = IntcodeState(read_input())
    inputs = deque()
    outputs = deque()

    prog = [
        "NOT A J",
        "NOT B T",
        "OR T J",
        "NOT C T",
        "OR T J",
        "AND D J",
        "WALK",
    ]

    inputs.extend(ord(x) for x in "".join(l + "\n" for l in prog))
    run_intcode(state, inputs, outputs)

    print("".join(gen_char_outputs(outputs)))


def part2():
    state = IntcodeState(read_input())
    inputs = deque()
    outputs = deque()

    prog = [
        # Jump if H empty, jump if A is empty
        "OR A J",
        "NOT J J",
        "NOT H T",
        "AND T J",
        # Simple Jump
        "NOT A T",
        "NOT T T",
        "AND B T",
        "AND C T",
        "NOT T T",
        "AND H T",
        # Combine jumps
        "OR T J",
        # Only jump if D is set
        "AND D J",
        # Run
        "RUN",
    ]

    assert len(prog) <= 15

    inputs.extend(ord(x) for x in "".join(l + "\n" for l in prog))
    run_intcode(state, inputs, outputs)

    print("".join(gen_char_outputs(outputs)))


if __name__ == "__main__":
    # part1()
    part2()
