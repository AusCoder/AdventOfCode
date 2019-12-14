import re
import math
import itertools
from pathlib import Path


class Moon:
    def __init__(self, pos):
        self.pos = tuple(pos)
        self.vel = (0, 0, 0)

    def __repr__(self):
        return f"{self.__class__.__name__}(pos={self.pos}, vel={self.vel})"

    def update_velocity(self, moon):
        updates = list(self.gen_vel_updates(moon))
        self.vel = tuple(x + u for x, u in zip(self.vel, updates))

    def gen_vel_updates(self, moon):
        for c1, c2 in zip(self.pos, moon.pos):
            if c1 == c2:
                yield 0
            elif c1 < c2:
                yield 1
            else:
                yield -1

    def update_position(self):
        self.pos = tuple(p + v for p, v in zip(self.pos, self.vel))

    def energy(self):
        return sum(abs(x) for x in self.pos) * sum(abs(x) for x in self.vel)


class MoonCoord:
    def __init__(self, pos):
        self.pos = pos
        self.vel = 0

    def update_velocity(self, other):
        if self.pos < other.pos:
            self.vel += 1
        elif self.pos > other.pos:
            self.vel -= 1

    def update_position(self):
        self.pos += self.vel


def gen_input():
    lines = Path("input/12.txt").read_text().strip().split("\n")
    for line in lines:
        m = re.match(r"<x=(.*), y=(.*), z=(.*)>", line)
        yield Moon([int(x) for x in m.groups()])


def update_moons(moons):
    for m1, m2 in itertools.combinations(moons, 2):
        m1.update_velocity(m2)
        m2.update_velocity(m1)

    for m in moons:
        m.update_position()


def part1():
    moons = list(gen_input())

    for _ in range(1000):
        update_moons(moons)

    print(sum(m.energy() for m in moons))


def coord_repeat_frequency(coords):
    states = set()
    i = 0
    while True:
        t = tuple((x.pos, x.vel) for x in coords)
        if t in states:
            break
        states.add(t)
        i += 1
        update_moons(coords)
    return i


def lcm(x, y):
    return x * y // math.gcd(x, y)


def part2():
    moons = list(gen_input())
    x_coords = [MoonCoord(m.pos[0]) for m in moons]
    y_coords = [MoonCoord(m.pos[1]) for m in moons]
    z_coords = [MoonCoord(m.pos[2]) for m in moons]

    x_freq = coord_repeat_frequency(x_coords)
    y_freq = coord_repeat_frequency(y_coords)
    z_freq = coord_repeat_frequency(z_coords)

    print(x_freq, y_freq, z_freq)
    print(lcm(lcm(x_freq, y_freq), z_freq))


if __name__ == "__main__":
    # part1()
    part2()
