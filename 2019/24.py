import itertools
from pathlib import Path


def read_input():
    return Path("input/24.txt").read_text().strip().split("\n")


def gen_initial_bug_positions(lines):
    """Bug is tuple (x, y)"""
    for y, line in enumerate(lines):
        for x, c in enumerate(line):
            if c == "#":
                yield (x, y)
            elif c in [".", "?"]:
                pass
            else:
                raise RuntimeError((line, c))


def gen_initial_bug_positions_recursive(lines):
    """Bug is tuple (lvl, (x, y))"""
    for bug in gen_initial_bug_positions(lines):
        yield 0, bug


def gen_base_adj_positions(pos):
    x, y = pos
    yield from [
        (x - 1, y),
        (x + 1, y),
        (x, y - 1),
        (x, y + 1),
    ]


def gen_adj_positions(pos):
    w, h = 5, 5
    posns = gen_base_adj_positions(pos)

    def is_valid(p):
        x, y = p
        return x >= 0 and y >= 0 and x < w and y < h

    yield from filter(is_valid, posns)


def gen_adj_positions_recursive(pos_with_lvl):
    yield from itertools.chain(
        _gen_adj_positions_recursive_same_lvl(pos_with_lvl),
        _gen_adj_positions_recursive_outer_lvl(pos_with_lvl),
        _gen_adj_positions_recursive_inner_lvl(pos_with_lvl),
    )


def _gen_adj_positions_recursive_same_lvl(pos_with_lvl):
    w, h = 5, 5
    lvl, pos = pos_with_lvl

    def is_valid_this_lvl(p):
        x, y = p
        return w > x >= 0 and h > y >= 0 and (x, y) != (2, 2)

    yield from (
        (lvl, p) for p in filter(is_valid_this_lvl, gen_base_adj_positions(pos))
    )


def _gen_adj_positions_recursive_outer_lvl(pos_with_lvl):
    w, h = 5, 5
    lvl, pos = pos_with_lvl
    new_lvl = lvl + 1
    for x, y in gen_base_adj_positions(pos):
        if x < 0:
            yield new_lvl, (1, 2)
        elif x >= w:
            yield new_lvl, (3, 2)
        elif y < 0:
            yield new_lvl, (2, 1)
        elif y >= h:
            yield new_lvl, (2, 3)


def _gen_adj_positions_recursive_inner_lvl(pos_with_lvl):
    w, h = 5, 5
    lvl, pos = pos_with_lvl
    new_lvl = lvl - 1

    if pos == (1, 2):
        yield from ((new_lvl, (0, y)) for y in range(h))
    elif pos == (3, 2):
        yield from ((new_lvl, (4, y)) for y in range(h))
    elif pos == (2, 1):
        yield from ((new_lvl, (x, 0)) for x in range(w))
    elif pos == (2, 3):
        yield from ((new_lvl, (x, 4)) for x in range(w))


def gen_next_round_bugs(bugs, gen_adj_positions_fn):
    for bug in bugs:
        adjacent_bugs = [p for p in gen_adj_positions_fn(bug) if p in bugs]
        if len(adjacent_bugs) == 1:
            yield bug

    adjacent_positions = itertools.chain.from_iterable(
        gen_adj_positions_fn(bug) for bug in bugs
    )
    for potential_bug in adjacent_positions:
        if potential_bug in bugs:
            continue
        adjacent_bugs = [p for p in gen_adj_positions_fn(potential_bug) if p in bugs]
        if len(adjacent_bugs) in [1, 2]:
            yield potential_bug


def calculate_biodiversity(bugs):
    biodiv = 0
    for x, y in bugs:
        biodiv += 2 ** (5 * y + x)
    return biodiv


def draw_bugs(bugs):
    grid = [["." for _ in range(5)] for _ in range(5)]
    for x, y in bugs:
        grid[y][x] = "#"
    print("\n".join("".join(r) for r in grid))


def part1():
    lines = read_input()
    bugs = set(gen_initial_bug_positions(lines))
    seen_configurations = [bugs]

    while True:
        bugs = set(gen_next_round_bugs(bugs, gen_adj_positions))
        if bugs in seen_configurations:
            break
        seen_configurations.append(bugs)
    draw_bugs(bugs)
    print(calculate_biodiversity(bugs))


def part2():
    lines = read_input()
    bugs = set(gen_initial_bug_positions_recursive(lines))

    for _ in range(200):
        bugs = set(gen_next_round_bugs(bugs, gen_adj_positions_recursive))

    print(len(bugs))


if __name__ == "__main__":
    # part1()
    part2()
