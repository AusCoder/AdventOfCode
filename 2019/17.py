from collections import deque
from pathlib import Path

from intcode import IntcodeState, InputRequiredError, run_intcode


def read_input():
    return [int(x) for x in Path("input/17.txt").read_text().strip().split(",")]


def gen_adj_positions(pos):
    x, y = pos
    yield (x - 1, y)
    yield (x + 1, y)
    yield (x, y - 1)
    yield (x, y + 1)


class Ascii:
    ROBOT_CODES = [ord(">"), ord("<"), ord("^"), ord("v"), ord("X")]

    def __init__(self, instructions):
        self.state = IntcodeState(instructions)
        self.inputs = deque()
        self.outputs = deque()

        self.scaffold = None
        self.robot_position = None
        self.robot_direction = None

    def __repr__(self):
        xs = [x for x, _ in self.scaffold]
        ys = [y for _, y in self.scaffold]
        x_min = min(xs)
        y_min = min(ys)

        grid = [
            ["." for _ in range(x_min, max(xs) + 1)] for _ in range(y_min, max(ys) + 1)
        ]
        for x, y in self.scaffold:
            grid[y - y_min][x - x_min] = "#"
        x, y = self.robot_position
        grid[y - y_min][x - x_min] = self.robot_direction
        for x, y in self.gen_intersections():
            grid[y - y_min][x - x_min] = "O"
        rendered_grid = "\n".join("".join(r) for r in grid)
        return rendered_grid

    def build_map(self):
        run_intcode(self.state, self.inputs, self.outputs)
        self.read_scaffold()

    def read_scaffold(self):
        self.scaffold = set()
        x, y = 0, 0
        prev_char = None
        c = None
        while True:
            prev_char = c
            c = self.outputs.popleft()
            if c in [ord("#"), *self.ROBOT_CODES]:
                if c in self.ROBOT_CODES:
                    self.robot_direction = chr(c)
                    self.robot_position = (x, y)
                self.scaffold.add((x, y))
                x += 1
            elif c == ord("."):
                x += 1
            elif c == ord("\n"):
                if prev_char == ord("\n"):
                    break
                y += 1
                x = 0
            else:
                raise RuntimeError(c)

    def burn_to_double(self):
        prev_char = None
        c = None
        while True:
            prev_char = c
            c = self.outputs.popleft()
            if c == ord("\n") and prev_char == ord("\n"):
                break

    def gen_intersections(self):
        for pos in self.scaffold:
            if all(neigh in self.scaffold for neigh in gen_adj_positions(pos)):
                yield pos

    def gen_alignment_parameters(self):
        for x, y in self.gen_intersections():
            yield x * y

    def traverse_map(self):
        # I got this by looking at the scaffold,
        # doing a bunch of counting, trying some things,
        # failing and trying some more.
        # I can't currently think of a nice way to discover
        # these programs programatically.
        # Perhaps generating possible strings of movements
        # around the grid and trying to decompose them as the
        # concatenation of 3 substrings?
        main = "A,B,B,C,C,A,B,B,C,A"
        a = "R,4,R,12,R,10,L,12"
        b = "L,12,R,4,R,12"
        c = "L,12,L,8,R,10"
        video_feed = "n"
        progs = [main, a, b, c, video_feed]
        assert all(len(p) <= 20 for p in progs)
        inpt = "".join(s + "\n" for s in progs)
        self.inputs.extend(ord(x) for x in inpt)

        run_intcode(self.state, self.inputs, self.outputs)

        self.read_scaffold()
        self.burn_to_double()
        self.read_scaffold()
        print(self.outputs)


def part1():
    ascii_ = Ascii(read_input())
    ascii_.build_map()
    print(ascii_)
    print(sum(ascii_.gen_alignment_parameters()))


def part2():
    instructions = read_input()
    instructions[0] = 2
    ascii_ = Ascii(instructions)
    ascii_.traverse_map()
    print(ascii_)
    # print(sum(ascii_.gen_alignment_parameters()))


if __name__ == "__main__":
    # part1()
    part2()
