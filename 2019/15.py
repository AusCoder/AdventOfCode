from collections import deque
from pathlib import Path

from intcode import IntcodeState, InputRequiredError, run_intcode


def read_input():
    return [int(x) for x in Path("input/15.txt").read_text().strip().split(",")]


def gen_adj_positions(position):
    x, y = position
    yield (x, y - 1)
    yield (x, y + 1)
    yield (x - 1, y)
    yield (x + 1, y)


class RepairRobot:
    def __init__(self, instructions):
        self.positon = (0, 0)
        self.state = IntcodeState(instructions)
        self.inputs = deque()
        self.outputs = deque()

    def move_to(self, dst):
        command = self.command_required(dst)
        return self.move(command)

    def move(self, command):
        self.inputs.append(command)
        try:
            run_intcode(self.state, self.inputs, self.outputs)
            raise RuntimeError
        except InputRequiredError:
            (status,) = self.outputs
            self.outputs.clear()
        if status == 0:
            return status
        elif status in [1, 2]:
            x, y = self.positon
            if command == 1:
                self.positon = (x, y - 1)
            elif command == 2:
                self.positon = (x, y + 1)
            elif command == 3:
                self.positon = (x - 1, y)
            elif command == 4:
                self.positon = (x + 1, y)
            else:
                raise RuntimeError
            return status
        raise RuntimeError

    def move_along_shortest_path_to(self, dst, pathwise_distances):
        while self.positon != dst:
            next_step = min(
                (p for p in self.gen_adj_positions() if p in pathwise_distances),
                key=lambda p: pathwise_distances[p],
            )
            if self.move_to(next_step) == 0:
                raise RuntimeError

    def command_required(self, dst):
        dx, dy = (a - b for a, b in zip(self.positon, dst))
        if dx == 0 and dy == -1:
            return 2
        elif dx == 0 and dy == 1:
            return 1
        elif dx == -1 and dy == 0:
            return 4
        elif dx == 1 and dy == 0:
            return 3
        raise RuntimeError((self.positon, dst))

    def gen_adj_positions(self):
        yield from gen_adj_positions(self.positon)

    def gen_unknown_adj_positions(self, game: "Game"):
        for pos in self.gen_adj_positions():
            if pos in game.walls:
                continue
            if pos in game.empty:
                continue
            yield pos


class Game:
    def __init__(self, instructions):
        self.robot = RepairRobot(instructions)
        self.walls = set()
        self.empty = set([(0, 0)])
        self.target = None

    def gen_posns_with_unknown_neighbors(self):
        for pos in self.empty:
            if any(
                p
                for p in gen_adj_positions(pos)
                if p not in self.walls.union(self.empty)
            ):
                yield pos

    def gen_empty_adj_positions(self, pos):
        for pos in gen_adj_positions(pos):
            if pos in self.empty:
                yield pos

    def known_path_distances_from(self, start):
        """Finds the shortest pathwise distance from the starting point
            to all known (ie empty) locations.
        """
        distances = dict()
        positions = deque()
        positions.append((start, 0))

        while positions:
            current_pos, current_dist = positions.popleft()
            distances[current_pos] = current_dist

            for pos in self.gen_empty_adj_positions(current_pos):
                if pos in distances:
                    continue
                if pos in set(p for p, _ in positions):
                    continue
                positions.append((pos, current_dist + 1))
        return distances

    def __repr__(self):
        xs = [x for x, _ in self.empty.union(self.walls)]
        ys = [y for _, y in self.empty.union(self.walls)]
        x_min = min(xs) - 2
        y_min = min(ys) - 2
        x_max = max(xs) + 2
        y_max = max(ys) + 2

        grid = [[" " for _ in range(x_min, x_max + 1)] for _ in range(y_min, y_max + 1)]
        for x, y in self.walls:
            grid[y - y_min][x - x_min] = "#"
        for x, y in self.empty:
            grid[y - y_min][x - x_min] = "."

        x, y = self.robot.positon
        grid[y - y_min][x - x_min] = "R"

        if self.target:
            x, y = self.target
            grid[y - y_min][x - x_min] = "T"

        rendered_grid = "\n".join("".join(r) for r in grid)
        return rendered_grid

    def discover(self):
        """Move to all unknown positions adjacent to current position"""
        start_pos = self.robot.positon
        for pos in self.robot.gen_unknown_adj_positions(self):
            status = self.robot.move_to(pos)
            if status == 0:
                self.walls.add(pos)
            elif status == 1:
                self.empty.add(pos)
                self.robot.move_to(start_pos)
            elif status == 2:
                self.empty.add(pos)
                self.target = pos
                self.robot.move_to(start_pos)
            else:
                raise RuntimeError(status)

    def run(self):
        while True:
            try:
                # This is making a bet that the maze eventually stops
                # It probably does...
                # We can probably stop earlier when we find the target
                dst, *_ = self.gen_posns_with_unknown_neighbors()
            except ValueError:
                break
            pathwise_distances = self.known_path_distances_from(dst)
            self.robot.move_along_shortest_path_to(dst, pathwise_distances)
            self.discover()

        if not self.target:
            raise RuntimeError


def part1():
    game = Game(read_input())
    game.run()
    pathwise_distances = game.known_path_distances_from((0, 0))
    print(game)
    print(pathwise_distances[game.target])


def part2():
    game = Game(read_input())
    game.run()
    pathwise_distances = game.known_path_distances_from(game.target)
    print(game)
    print(max(pathwise_distances.values()))


if __name__ == "__main__":
    # part1()
    part2()
