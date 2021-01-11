import itertools
from pathlib import Path


def gen_input():
    lines = Path("input/day17.txt").read_text().strip().split("\n")
    for line in lines:
        coords = (x.strip().split("=") for x in line.split(","))
        yield ScanResult(**dict(coords))


class ScanResult:
    def __init__(self, x, y):
        self.x = self._pts_from_input(x)
        self.y = self._pts_from_input(y)

    def _pts_from_input(self, inpt):
        if ".." in inpt:
            start, stop = (int(x) for x in inpt.split(".."))
            return list(range(start, stop + 1))
        return [int(inpt)]

    def __repr__(self):
        return f"{self.__class__.__name__}(x={self.x}, y={self.y})"

    def gen_points(self):
        return itertools.product(self.x, self.y)


class Terrain:
    def __init__(self, scan_results):
        self.scan_results = list(scan_results)

        water_x = 500
        water_y = 0

        xs = list(itertools.chain.from_iterable(r.x for r in self.scan_results))
        ys = list(itertools.chain.from_iterable(r.y for r in self.scan_results))
        x_min = min(xs) - 1
        x_max = max(xs) + 1
        y_max = max(ys)

        self.grid = [["." for _ in range(x_min, x_max + 1)] for _ in range(y_max + 1)]
        for x, y in itertools.chain.from_iterable(
            r.gen_points() for r in self.scan_results
        ):
            self.grid[y][x - x_min] = "#"

        self.grid[water_y][water_x - x_min] = "+"

    # def flow_from_point(self, x, y):
    #     if self.grid[y + 1][x] == ".":
    #         self.grid[y + 1][x] = "|"
    #         self.flow_from_point(self, x, y + 1)
    #         return

    def draw(self):
        for row in self.grid:
            print("".join(row))


def part1():
    terrain = Terrain(gen_input())
    terrain.draw()

    # for x in gen_input():
    #     print(x)


if __name__ == "__main__":
    part1()
