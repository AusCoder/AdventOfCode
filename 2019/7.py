import itertools
from collections import deque
from pathlib import Path

from intcode import run_intcode, IntcodeState, InputRequiredError


def run_amplifiers_part1(instructions, phases):
    output = 0
    for phase in phases:
        inputs = deque([phase, output])
        outputs = deque()
        run_intcode(IntcodeState(instructions), inputs, outputs)
        (output,) = outputs
    return output


def part1():
    instructions = [int(x) for x in Path("input/7.txt").read_text().strip().split(",")]

    amplitudes = (
        run_amplifiers_part1(instructions, phase)
        for phase in itertools.permutations(range(5))
    )
    print(max(amplitudes))


def run_amplifiers_part2(instructions, phases):
    NAMES = "ABCDE"
    states = {
        NAMES[i]: (IntcodeState(instructions), deque([phase]))
        for i, phase in enumerate(phases)
    }

    _, initial_inputs = states["A"]
    initial_inputs.append(0)

    amplifier_idx = 0
    while any(not state.is_halted for state, _ in states.values()):
        cur_amplifier = NAMES[amplifier_idx % 5]
        next_amplifier = NAMES[(amplifier_idx + 1) % 5]
        state, inputs = states[cur_amplifier]
        _, next_inputs = states[next_amplifier]

        try:
            run_intcode(state, inputs, next_inputs)
        except InputRequiredError:
            pass

        amplifier_idx += 1
    _, (output,) = states["A"]
    return output


def part2():
    instructions = [int(x) for x in Path("input/7.txt").read_text().strip().split(",")]

    amplitudes = (
        run_amplifiers_part2(instructions, phase)
        for phase in itertools.permutations(range(5, 10))
    )
    print(max(amplitudes))


if __name__ == "__main__":
    # part1()
    part2()
