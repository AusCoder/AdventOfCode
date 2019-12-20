import string
from collections import deque
from pathlib import Path


def read_input():
    return [l for l in Path("input/20.txt").read_text().split("\n") if l]


class Map:
    def __init__(self, lines):
        self.walls = set()
        self.empty = set()
        self.portals = dict()

        self.portals_is_outer = dict()

        pending_portals = dict()

        for y, line in enumerate(lines):
            for x, c in enumerate(line):
                if c == "#":
                    self.walls.add((x, y))
                elif c == ".":
                    self.empty.add((x, y))
                    portal_entrance, portal_name, is_outer = self._check_portal(
                        lines, x, y
                    )
                    if not portal_entrance:
                        continue
                    self.portals_is_outer[portal_entrance] = is_outer
                    if portal_name in pending_portals:
                        other_entrance = pending_portals.pop(portal_name)
                        self.portals[portal_entrance] = other_entrance
                        self.portals[other_entrance] = portal_entrance
                    else:
                        pending_portals[portal_name] = portal_entrance
                elif c in string.ascii_uppercase:
                    pass
                elif c == " ":
                    pass
                else:
                    raise RuntimeError

        self.entrance = pending_portals["AA"]
        self.exit = pending_portals["ZZ"]

        self.entrance_with_lvl = (self.entrance, 0)
        self.exit_with_lvl = (self.exit, 0)

    def _check_portal(self, lines, x, y):
        diffs = [
            (-1, 0),
            (1, 0),
            (0, -1),
            (0, 1),
        ]
        portal_entrance = None
        portal_name = None
        for dx, dy in diffs:
            try:
                c = lines[y + dy][x + dx]
            except IndexError:
                continue
            if c in string.ascii_uppercase:
                portal_entrance = (x, y)
                c2 = lines[y + 2 * dy][x + 2 * dx]
                if dx < 0 or dy < 0:
                    portal_name = c2 + c
                else:
                    portal_name = c + c2
                break

        # Check whether portal is on the outer ring
        width_line = lines[len(lines) // 2]
        if width_line[-1] in string.ascii_uppercase:
            width_line = width_line[:-2]
        width = len(width_line)
        height = len(lines) - 2
        if x <= 2 or y <= 2 or width - x <= 2 or height - y <= 2:
            is_outer = True
        else:
            is_outer = False

        return portal_entrance, portal_name, is_outer

    def gen_adj_positions(self, pos):
        x, y = pos
        posns = [
            (x - 1, y),
            (x + 1, y),
            (x, y - 1),
            (x, y + 1),
        ]
        for p in posns:
            if p in self.empty:
                yield p
        if pos in self.portals:
            yield self.portals[pos]

    def _bfs(self, start_pos, adj_posns_fn, stop_pos=None):
        distances = dict()
        positions = deque()
        positions.append((start_pos, 0))

        while positions:
            pos, dist = positions.popleft()
            distances[pos] = dist
            if pos == stop_pos:
                break
            for next_pos in adj_posns_fn(pos):
                if next_pos in distances:
                    continue
                if next_pos in set(p for p, _ in positions):
                    continue
                positions.append((next_pos, dist + 1))
        return distances

    def shortest_path_through_maze(self):
        distances = self._bfs(self.entrance, self.gen_adj_positions)
        print(distances[self.exit])

    def gen_adj_positions_with_levels(self, pos_with_lvl):
        pos, lvl = pos_with_lvl
        x, y = pos

        posns = [
            ((x - 1, y), lvl),
            ((x + 1, y), lvl),
            ((x, y - 1), lvl),
            ((x, y + 1), lvl),
        ]
        for p, l in posns:
            if p in self.empty:
                yield p, l

        if pos in self.portals and self.portals_is_outer[pos] and lvl > 0:
            yield self.portals[pos], lvl - 1
        elif pos in self.portals and not self.portals_is_outer[pos]:
            yield self.portals[pos], lvl + 1

    def shortest_path_through_maze_with_levels(self):
        distances = self._bfs(
            self.entrance_with_lvl,
            self.gen_adj_positions_with_levels,
            stop_pos=self.exit_with_lvl,
        )
        print(distances[self.exit_with_lvl])


def part1():
    m = Map(read_input())
    m.shortest_path_through_maze()


def part2():
    m = Map(read_input())
    m.shortest_path_through_maze_with_levels()


if __name__ == "__main__":
    part1()
    part2()
