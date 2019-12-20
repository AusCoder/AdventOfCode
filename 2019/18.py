import string
import functools
from collections import deque, defaultdict
from pathlib import Path


def read_input():
    return Path("input/18.txt").read_text().strip().split("\n")


def gen_adj_positions(pos):
    x, y = pos
    yield (x - 1, y)
    yield (x + 1, y)
    yield (x, y - 1)
    yield (x, y + 1)


def gen_eventually_reachable_positions(pos, walls):
    for p in gen_adj_positions(pos):
        if p not in walls:
            yield p


def bfs(start_pos, walls, doors):
    distances = dict()
    positions = deque()

    positions.append((start_pos, 0, []))
    while positions:
        cur_pos, dist, cur_doors_required = positions.popleft()
        distances[cur_pos] = (dist, cur_doors_required)
        for p in gen_eventually_reachable_positions(cur_pos, walls):
            if p in distances:
                continue
            if p in set(p for p, _, _ in positions):
                continue
            if p in doors:
                doors_required = [*cur_doors_required, doors[p]]
            else:
                doors_required = cur_doors_required
            positions.append((p, dist + 1, doors_required))
    return distances


def calculate_distances_between_keys(walls, keys, doors, start_pos):
    distances = {}
    for start in keys:
        distances_to_all_points = bfs(start, walls, doors)
        distances[start] = {
            p: d for p, d in distances_to_all_points.items() if p in keys
        }
    distances_to_all_points = bfs(start_pos, walls, doors)
    distances[start_pos] = {
        p: d for p, d in distances_to_all_points.items() if p in keys
    }
    return distances


def count_steps_multiple_players(distances_per_player, keys, start_posns):
    keys = tuple((p, c) for p, c in keys.items())
    start_posns = tuple(start_posns)

    @functools.lru_cache(maxsize=None)
    def _count_steps(keys, posns):
        if not keys:
            return 0

        key_posns = [p for p, _ in keys]
        key_names = [n for _, n in keys]

        possible_moves = []
        for player, pos in enumerate(posns):
            for dst, (dist, req_doors) in distances_per_player[player][pos].items():
                if dst not in key_posns:
                    continue
                if [d for d in req_doors if d.lower() in key_names]:
                    continue
                possible_moves.append((player, dst, dist))

        return min(gen_possible_outcomes(keys, posns, possible_moves))

    def gen_possible_outcomes(keys, posns, possible_moves):
        for player, dst, dist in possible_moves:
            ps = list(posns)
            ps[player] = dst
            ps = tuple(ps)
            ks = tuple((p, c) for p, c in keys if p != dst)
            yield dist + _count_steps(ks, ps)

    return _count_steps(keys, start_posns)


class Map:
    def __init__(self, lines):
        self.walls = set()
        self.empty = set()

        self.keys = dict()
        self.doors = dict()

        self.start_posns = []

        for y, line in enumerate(lines):
            for x, c in enumerate(line):
                if c == "#":
                    self.walls.add((x, y))
                elif c == ".":
                    self.empty.add((x, y))
                elif c in string.ascii_lowercase:
                    self.empty.add((x, y))
                    self.keys[(x, y)] = c
                elif c in string.ascii_uppercase:
                    self.empty.add((x, y))
                    self.doors[(x, y)] = c
                elif c == "@":
                    self.start_posns.append((x, y))
                else:
                    raise RuntimeError(c)

    def fill_for_part2(self):
        ((x, y),) = self.start_posns

        posns_to_fill = [
            (x, y),
            (x - 1, y),
            (x + 1, y),
            (x, y - 1),
            (x, y + 1),
        ]
        for p in posns_to_fill:
            self.walls.add(p)
            try:
                self.empty.remove(p)
            except KeyError:
                pass

        self.start_posns = [
            (x - 1, y - 1),
            (x + 1, y - 1),
            (x - 1, y + 1),
            (x + 1, y + 1),
        ]

    def __repr__(self):
        xs = [x for x, _ in self.walls]
        ys = [y for _, y in self.walls]
        grid = [["." for _ in range(max(xs) + 1)] for _ in range(max(ys) + 1)]

        for x, y in self.walls:
            grid[y][x] = "#"
        for x, y in self.start_posns:
            grid[y][x] = "@"

        for (x, y), c in self.doors.items():
            grid[y][x] = c
        for (x, y), c in self.keys.items():
            grid[y][x] = c
        return "\n".join("".join(l) for l in grid)


def count_steps_in_map(m):
    distances_per_player = {
        i: calculate_distances_between_keys(m.walls, m.keys, m.doors, start_pos)
        for i, start_pos in enumerate(m.start_posns)
    }
    steps = count_steps_multiple_players(distances_per_player, m.keys, m.start_posns)
    print(steps)


def part1():
    m = Map(read_input())
    print(m)
    count_steps_in_map(m)


def part2():
    m = Map(read_input())
    m.fill_for_part2()
    print(m)
    count_steps_in_map(m)


if __name__ == "__main__":
    # part1()
    part2()
