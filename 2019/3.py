import re
from collections import defaultdict
from pathlib import Path

line1, line2 = [
    x.split(",") for x in Path("input/3.txt").read_text().strip().split("\n")
]


def gen_posns(instrs):
    x, y, steps = 0, 0, 0
    for instr in instrs:
        direction = instr[0]
        count = int(instr[1:])
        for _ in range(count):
            steps += 1
            if direction == "R":
                x += 1
            elif direction == "L":
                x -= 1
            elif direction == "U":
                y += 1
            elif direction == "D":
                y -= 1
            else:
                raise RuntimeError
            yield x, y, steps


def dist_to_orig(pos):
    x, y = pos
    return abs(x) + abs(y)


def part1():
    posns1 = set((x, y) for x, y, _ in gen_posns(line1))
    intersections = ((x, y) for x, y, _ in gen_posns(line2) if (x, y) in posns1)
    print(dist_to_orig(min(intersections, key=dist_to_orig)))


def part2():
    posns1 = defaultdict(list)
    for x, y, s in gen_posns(line1):
        posns1[(x, y)].append(s)
    posns1 = {k: min(v) for k, v in posns1.items()}

    posns2 = defaultdict(list)
    for x, y, s in gen_posns(line2):
        posns2[(x, y)].append(s)
    posns2 = {k: min(v) for k, v in posns2.items()}

    min_steps = min(v + posns2[k] for k, v in posns1.items() if k in posns2)
    print(min_steps)


part1()
part2()
