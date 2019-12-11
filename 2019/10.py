import math
import operator
from collections import defaultdict, deque
from pathlib import Path


class Vec2D:
    def __init__(self, x: int, y: int) -> None:
        self.x = x
        self.y = y

    def to_normalized(self) -> "Vec2D":
        return self // math.gcd(self.x, self.y)

    def __eq__(self, other: "Vec2D") -> bool:
        return self.x == other.x and self.y == other.y

    def __lt__(self, other: "Vec2D") -> bool:
        """Order points in a radial expanding order

        Remember that in our case, the ys get positive as you go down,
        so the unit vectors in each direction are
                    (0, -1)
        (-1, 0)                 (1, 0)
                    (0, 1)
        """
        if self.x >= 0 and other.x < 0:
            return True
        if self.x < 0 and other.x >= 0:
            return False
        if self.x == 0 and other.x == 0:
            if self.y >= 0 or other.y >= 0:
                return self.y < other.y
            return other.y < self.y
        det = self.x * other.y - self.y * other.x
        if det < 0:
            return False
        elif det > 0:
            return True
        return self.norm_sqrd() < other.norm_sqrd()

    def norm_sqrd(self) -> int:
        return self.x ** 2 + self.y ** 2

    def __hash__(self) -> int:
        return hash((self.x, self.y))

    def __floordiv__(self, n: int) -> "Vec2D":
        return Vec2D(self.x // n, self.y // n)

    def __sub__(self, other: "Vec2D") -> "Vec2D":
        return Vec2D(self.x - other.x, self.y - other.y)

    def __add__(self, other: "Vec2D") -> "Vec2D":
        return Vec2D(self.x + other.x, self.y + other.y)

    def __repr__(self):
        return f"{self.__class__.__name__}(x={self.x}, y={self.y})"


v1 = Vec2D(0, -1)
v2 = Vec2D(1, 0)
v3 = Vec2D(0, 1)
v4 = Vec2D(-1, 0)

assert v1 < v2
assert v1 < v3
assert v2 < v3
assert v2 < v4
assert v3 < v4


def gen_location_and_num_in_sight(asteroid_positions):
    for possible_location in asteroid_positions:
        num_in_sight = len(
            set(
                (pos - possible_location).to_normalized()
                for pos in asteroid_positions
                if pos != possible_location
            )
        )
        yield possible_location, num_in_sight


def read_asertoid_positions():
    lines = Path("input/10.txt").read_text().strip().split("\n")
    return [
        Vec2D(x, y)
        for y, line in enumerate(lines)
        for x, char in enumerate(line)
        if char == "#"
    ]


def part1():
    asteroid_positions = read_asertoid_positions()
    best_location, num_in_sight = max(
        gen_location_and_num_in_sight(asteroid_positions), key=operator.itemgetter(1)
    )
    print(best_location, num_in_sight)


def part2():
    asteroid_positions = read_asertoid_positions()
    best_location, _ = max(
        gen_location_and_num_in_sight(asteroid_positions), key=operator.itemgetter(1)
    )

    rays_from_best_location = defaultdict(list)
    for pos in asteroid_positions:
        if pos == best_location:
            continue
        rays_from_best_location[(pos - best_location).to_normalized()].append(
            pos - best_location
        )

    rays_from_best_location = (
        (k, deque(sorted(v))) for k, v in rays_from_best_location.items()
    )
    rays_from_best_location = sorted(
        rays_from_best_location, key=operator.itemgetter(0)
    )

    cur_idx = 0
    for _ in range(200):
        if all(not d for _, d in rays_from_best_location):
            raise RuntimeError("out of asteroids")
        while True:
            _, asteroid_vecs = rays_from_best_location[cur_idx]
            if asteroid_vecs:
                break
            cur_idx = (cur_idx + 1) % len(rays_from_best_location)
        last_destoyed_asteroid_vec = asteroid_vecs.popleft()
        cur_idx = (cur_idx + 1) % len(rays_from_best_location)

    ast = last_destoyed_asteroid_vec + best_location
    print(ast.x * 100 + ast.y)


if __name__ == "__main__":
    # part1()
    part2()
