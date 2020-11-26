# addi 0 7 3
# op   A B C

import re
from pathlib import Path
from typing import List, Union


OP_CODE_NAMES = [
    "addr",
    "addi",
    "mulr",
    "muli",
    "banr",
    "bani",
    "borr",
    "bori",
    "setr",
    "seti",
    "gtir",
    "gtri",
    "gtrr",
    "eqir",
    "eqri",
    "eqrr",
]


def run(registers: List[int], instruction: List[Union[str, int]]):
    op_code_name, a, b, c = instruction
    if op_code_name == "addr":
        registers[c] = registers[a] + registers[b]
    elif op_code_name == "addi":
        registers[c] = registers[a] + b
    elif op_code_name == "mulr":
        registers[c] = registers[a] * registers[b]
    elif op_code_name == "muli":
        registers[c] = registers[a] * b
    elif op_code_name == "banr":
        registers[c] = registers[a] & registers[b]
    elif op_code_name == "bani":
        registers[c] = registers[a] & b
    elif op_code_name == "borr":
        registers[c] = registers[a] | registers[b]
    elif op_code_name == "bori":
        registers[c] = registers[a] | b
    elif op_code_name == "setr":
        registers[c] = registers[a]
    elif op_code_name == "seti":
        registers[c] = a
    elif op_code_name == "gtir":
        registers[c] = 1 if a > registers[b] else 0
    elif op_code_name == "gtri":
        registers[c] = 1 if registers[a] > b else 0
    elif op_code_name == "gtrr":
        registers[c] = 1 if registers[a] > registers[b] else 0
    elif op_code_name == "eqir":
        registers[c] = 1 if a == registers[b] else 0
    elif op_code_name == "eqri":
        registers[c] = 1 if registers[a] == b else 0
    elif op_code_name == "eqrr":
        registers[c] = 1 if registers[a] == registers[b] else 0
    else:
        raise RuntimeError(instruction)


LIST_REGEX = r"\[(-?\d+), (-?\d+), (-?\d+), (-?\d+)\]"


def gen_before_after(text):
    lines = text.strip().split("\n")
    grouped = (lines[n : n + 4] for n in range(0, len(lines), 4))
    for group in grouped:
        line1, line2, line3, *_ = group
        before_match = re.match(rf"Before:\s*{LIST_REGEX}", line1)
        if not before_match:
            return
        registers_before = [int(x) for x in before_match.groups()]
        instruction = [int(x) for x in line2.split(" ")]
        registers_after = [
            int(x) for x in re.match(rf"After:\s*{LIST_REGEX}", line3).groups()
        ]
        yield registers_before, instruction, registers_after


def gen_matching_op_codes(
    possible_op_code_names, current_before, current_instruction, after
):
    for op_code_name in possible_op_code_names:
        instruction = list(current_instruction)
        instruction[0] = op_code_name
        before = list(current_before)
        run(before, instruction)
        if before == after:
            yield op_code_name


def part1():
    before_and_afters = gen_before_after(Path("input/day16.txt").read_text())

    behaves_like_3_op_codes = 0
    for before, instruction, after in before_and_afters:
        num_matching_op_codes = sum(
            1 for _ in gen_matching_op_codes(OP_CODE_NAMES, before, instruction, after)
        )
        if num_matching_op_codes >= 3:
            behaves_like_3_op_codes += 1
    print(behaves_like_3_op_codes)


def gen_program(text):
    skip_count = 0
    for line in text.strip().split("\n"):
        if skip_count > 0:
            skip_count -= 1
            continue
        if line.startswith("Before"):
            skip_count = 3
            continue
        if not line:
            continue
        yield [int(x) for x in line.strip().split(" ")]


def part2():
    before_and_afters = list(gen_before_after(Path("input/day16.txt").read_text()))

    op_code_mapping = {}

    while len(op_code_mapping) < len(OP_CODE_NAMES):
        for before, instruction, after in before_and_afters:
            op_code, *_ = instruction
            if op_code in op_code_mapping:
                continue
            unmatched_op_code_names = [
                n for n in OP_CODE_NAMES if n not in op_code_mapping.values()
            ]
            try:
                (matching_op_code_name,) = gen_matching_op_codes(
                    unmatched_op_code_names, before, instruction, after
                )
                op_code_mapping[op_code] = matching_op_code_name
            except ValueError:
                pass

    registers = [0] * 4
    program = gen_program(Path("input/day16.txt").read_text())
    for instruction in program:
        instruction[0] = op_code_mapping[instruction[0]]
        run(registers, instruction)
    print(registers[0])


if __name__ == "__main__":
    # part1()
    part2()
