import itertools
from collections import deque
from pathlib import Path
from typing import Dict, List, Iterable, Set, Optional


class NoEnemiesExistError(Exception):
    pass


class Position:
    def __init__(self, x: int, y: int) -> None:
        self.x = x
        self.y = y

    def __repr__(self):
        return f"({self.x}, {self.y})"

    def __lt__(self, other):
        if self.y == other.y:
            return self.x < other.x
        return self.y < other.y

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y

    def __hash__(self):
        return hash((self.x, self.y))

    def empty_adj_positions(self, game: "Game") -> Iterable["Position"]:
        """Returns all positions that are empty and adjacent to self.
        """
        occupied_positions = set(c.position for c in game.creatures)
        posns = self.adj_positions(game.map)
        posns = (p for p in posns if not game.map.is_wall(p))
        posns = (p for p in posns if p not in occupied_positions)
        return posns

    def adj_positions(self, m: "Map") -> Iterable["Position"]:
        if self.y - 1 >= 0:
            yield Position(self.x, self.y - 1)
        if self.x - 1 >= 0:
            yield Position(self.x - 1, self.y)
        if self.x + 1 < m.width:
            yield Position(self.x + 1, self.y)
        if self.y + 1 < m.height:
            yield Position(self.x, self.y + 1)

    def reachable_positions_with_distance(self, game: "Game") -> Dict["Position", int]:
        """Returns all positions reachable from this position along with their
            shortest path distance to this position.
        """
        reachable = dict()
        enqued = (
            set()
        )  # substitute for not having a visited field on each node (ie Position)
        positions = deque()
        positions.append((self, 0))

        while positions:
            position, dist = positions.popleft()
            reachable[position] = dist
            for adj_position in position.empty_adj_positions(game):
                if adj_position in enqued:
                    continue
                enqued.add(adj_position)
                if adj_position not in reachable:
                    positions.append((adj_position, dist + 1))
        return reachable


class VisitablePosition(Position):
    def __init__(self, position: Position) -> None:
        super().__init__(position.x, position.y)
        self.visited = False


class Creature:
    def __init__(self, x: int, y: int) -> None:
        self.position = Position(x, y)
        self.hp = 200
        self.attack_power = 3

    def __repr__(self):
        return f"{self.__class__.__name__}(position={self.position}, hp={self.hp})"

    def __lt__(self, other):
        assert self.__class__ == other.__class__
        if self.hp == other.hp:
            return self.position < other.position
        return self.hp < other.hp

    def is_friend(self, other: "Creature") -> bool:
        return isinstance(other, self.__class__)

    def is_enemy(self, other: "Creature") -> bool:
        return not self.is_friend(other)

    def is_adjacent_to_enemy(self, game: "Game") -> bool:
        return bool(self.adjacent_enemies(game))

    def adjacent_enemies(self, game: "Game") -> List["Creature"]:
        enemies = set(c for c in game.creatures if c.is_enemy(self))
        if not enemies:
            raise NoEnemiesExistError
        adj_positions = list(self.position.adj_positions(game.map))
        return [e for e in enemies if e.position in adj_positions]

    def move(self, game: "Game") -> None:
        targets = [c for c in game.creatures if c.is_enemy(self)]
        if not targets:
            raise NoEnemiesExistError
        # Calculate destination
        in_range = set(
            itertools.chain.from_iterable(
                t.position.empty_adj_positions(game) for t in targets
            )
        )
        in_range_reachable_with_distance = {
            p: d
            for p, d in self.position.reachable_positions_with_distance(game).items()
            if p in in_range
        }
        if not in_range_reachable_with_distance:
            return

        min_dist = min(in_range_reachable_with_distance.values())
        nearest_in_range_reachable = [
            p for p, d in in_range_reachable_with_distance.items() if d == min_dist
        ]
        destination = min(nearest_in_range_reachable)
        # Calculate the first step toward the destination
        distances_to_destinaton = destination.reachable_positions_with_distance(game)
        possible_next_posns = [
            (p, d)
            for p, d in distances_to_destinaton.items()
            if p in set(self.position.empty_adj_positions(game))
        ]
        closest_next_posns = (
            p
            for p, d in possible_next_posns
            if d == min(d for _, d in possible_next_posns)
        )
        self.position = min(closest_next_posns)

    def attack(self, game: "Game") -> None:
        target = min(self.adjacent_enemies(game))
        target.hp -= self.attack_power
        if target.hp <= 0:
            game.remove_creature(target)


class Goblin(Creature):
    pass


class Elf(Creature):
    pass


class Map:
    def __init__(self, width: int, height: int, tiles: List[str]) -> None:
        self.width = width
        self.height = height
        self.tiles = tiles

    def __repr__(self):
        nl = "\n"
        return f"{nl.join(''.join(l) for l in self.tiles)}"

    def is_wall(self, position: Position) -> bool:
        return self.tiles[position.y][position.x] == "#"


class Game:
    def __init__(self, m: Map, elves: List[Elf], goblins: List[Goblin]) -> None:
        self.map = m
        self.elves = elves
        self.goblins = goblins
        self.completed_rounds = 0

    @property
    def creatures(self):
        return self.elves + self.goblins

    def remove_creature(self, creature: Creature) -> None:
        if creature in self.elves:
            self.elves.remove(creature)
        elif creature in self.goblins:
            self.goblins.remove(creature)
        else:
            raise RuntimeError("unknown creature")

    def __repr__(self):
        tiles = [list(t) for t in self.map.tiles]
        for e in self.elves:
            tiles[e.position.y][e.position.x] = "E"
        for g in self.goblins:
            tiles[g.position.y][g.position.x] = "G"
        m = "\n".join("".join(l) for l in tiles)

        es = "\n".join(repr(e) for e in self.elves)
        gs = "\n".join(repr(g) for g in self.goblins)
        return f"Game(completed_grounds={self.completed_rounds}):\n{m}\nElves:\n{es}\nGoblins:\n{gs}"

    def run_round(self):
        ordered_creatures = sorted(self.creatures, key=lambda c: c.position)
        for c in ordered_creatures:
            if c not in self.creatures:
                continue
            if not c.is_adjacent_to_enemy(self):
                c.move(self)
            if c.is_adjacent_to_enemy(self):
                c.attack(self)

        self.completed_rounds += 1

        # print(self)
        # print()

    def run(self):
        while True:
            try:
                self.run_round()
            except NoEnemiesExistError:
                break
        outcome = sum(c.hp for c in self.creatures) * self.completed_rounds
        print(outcome)

    def draw_with_positions(self, positions=None, char: str = "@") -> bool:
        positions = [] if positions is None else positions
        tiles = [list(t) for t in self.map.tiles]
        for e in self.elves:
            tiles[e.position.y][e.position.x] = "E"
        for g in self.goblins:
            tiles[g.position.y][g.position.x] = "G"
        if isinstance(positions, dict):
            for p, d in positions.items():
                tiles[p.y][p.x] = str(d) if d < 10 else "abcdefghijklmno"[d - 10]
        else:
            for p in positions:
                tiles[p.y][p.x] = char
        for l in tiles:
            print("".join(l))


def read_input(path: Path):
    tiles = [list(line) for line in path.read_text().strip().split("\n")]
    height = len(tiles)
    width = len(tiles[0])

    elves, goblins = [], []
    for y, line in enumerate(tiles):
        for x, tile in enumerate(line):
            if tile == "E":
                elves.append(Elf(x, y))
                line[x] = "."
            elif tile == "G":
                goblins.append(Goblin(x, y))
                line[x] = "."
            elif tile in [".", "#"]:
                pass
            else:
                raise RuntimeError

    return Game(Map(width, height, tiles), elves, goblins)


def part1():
    game = read_input(Path("input/day15.txt"))
    # print(game)
    # print()
    game.run()
    # game.run_round()
    # game.run_round()
    # game.run_round()


if __name__ == "__main__":
    part1()
