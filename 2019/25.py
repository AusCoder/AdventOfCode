import re
import operator
import itertools
from collections import deque
from pathlib import Path

from intcode import IntcodeState, InputRequiredError, run_intcode


def read_input():
    return [int(x) for x in Path("input/25.txt").read_text().strip().split(",")]


def gen_all_combinations(xs):
    for r in range(1, len(xs) + 1):
        yield from itertools.combinations(xs, r)


class NoUnknownAdjRoomsError(Exception):
    pass


SECURITY_CHECKPOINT = "Security Checkpoint"
PRESSURE_SENSITIVE_FLOOR = "Pressure-Sensitive Floor"


class Robot:
    def __init__(self, instructions):
        self.state = IntcodeState(instructions)
        self.inputs = deque()
        self.outputs = deque()

        self.current_room = None
        self.room_info = dict()
        self.room_connections = dict()

    def try_items_with_pressure_sensitive_floor(self):
        self._walk_ship()
        self.move_to_room(SECURITY_CHECKPOINT)
        all_items = list(self.gen_current_items())
        self.drop_all_items()
        for items in gen_all_combinations(all_items):
            for item in items:
                self.pick_up_item(item)
            self.move_to_pressure_sensitive_floor()
            if self.current_room == PRESSURE_SENSITIVE_FLOOR:
                break
            self.drop_all_items()
        print(self.current_room)

    def _walk_ship(self):
        self.move(None)
        self.take_all_items()

        while True:
            while True:
                try:
                    self.explore_unknown_adj_room()
                except NoUnknownAdjRoomsError:
                    break
            try:
                room, *_ = (
                    r
                    for r, neighbors in self.room_connections.items()
                    if any(x is None for x in neighbors.values())
                )
            except ValueError:
                break
            self.move_to_room(room)

    def explore_unknown_adj_room(self):
        try:
            (direction, _), *_ = (
                (d, r)
                for d, r in self.room_connections[self.current_room].items()
                if r is None
            )
        except ValueError:
            raise NoUnknownAdjRoomsError
        self.move(direction)
        self.take_all_items()

    def move_to_room(self, dst_room):
        distances = self._bfs(dst_room)
        while self.current_room != dst_room:
            direction, adj_room, _ = min(
                (
                    (direction, adj_room, distances[adj_room])
                    for direction, adj_room in self.room_connections[
                        self.current_room
                    ].items()
                    if adj_room is not None
                ),
                key=operator.itemgetter(2),
            )
            self.move(direction)
            assert adj_room == self.current_room

    def move_to_pressure_sensitive_floor(self):
        (direction,) = (
            d
            for d, n in self.room_connections[self.current_room].items()
            if n == PRESSURE_SENSITIVE_FLOOR
        )
        self.move(direction)

    def _bfs(self, start_room):
        rooms = deque()
        distances = dict()
        rooms.append((start_room, 0))

        while rooms:
            room, dist = rooms.popleft()
            distances[room] = dist
            for _, connecting_room in self.room_connections[room].items():
                if connecting_room is None:
                    continue
                if connecting_room in distances:
                    continue
                # if connecting_room in set(r for r, _ in rooms):
                #     continue
                rooms.append((connecting_room, dist + 1))
        return distances

    def _run_command(self, command):
        if command:
            self.validate_command(command)
            self.inputs.extend(ord(x) for x in command)
            self.inputs.append(ord("\n"))
        try:
            run_intcode(self.state, self.inputs, self.outputs)
        except InputRequiredError:
            pass
        text = "".join(chr(x) for x in self.outputs)
        self.outputs.clear()
        print(text)
        return text.split("\n")

    def move(self, command):
        prev_room = self.current_room
        lines = self._run_command(command)

        if command is None:
            self.update_no_command(lines)
        if command in ["north", "south", "east", "west"]:
            self.update_movement_command(lines, prev_room, command)
        # self.take_all_items()

    def update_movement_command(self, lines, prev_room, command):
        try:
            ((room_name, doors, items),) = self.gen_room_info(lines)
            self.room_connections[prev_room][command] = room_name
            self.current_room = room_name
            if room_name not in self.room_info:
                self.room_info[room_name] = (doors, items, " ".join(lines))
            if room_name not in self.room_connections:
                self.room_connections[room_name] = {d: None for d in doors}
                self.room_connections[room_name][
                    self.opposite_direction(command)
                ] = prev_room
        except ValueError:
            (
                (room_name1, doors1, items1),
                (room_name2, doors2, items2),
            ) = self.gen_room_info(lines)
            assert room_name1 == "Pressure-Sensitive Floor"
            assert room_name2 == "Security Checkpoint"
            assert prev_room == room_name2
            self.room_connections[prev_room][command] = room_name1
            self.current_room = room_name2
            if room_name1 not in self.room_info:
                self.room_info[room_name1] = (doors1, items1, " ".join(lines))
            if room_name1 not in self.room_connections:
                self.room_connections[room_name1] = {d: None for d in doors1}
                self.room_connections[room_name1][
                    self.opposite_direction(command)
                ] = prev_room

    def update_no_command(self, lines):
        ((room_name, doors, items),) = self.gen_room_info(lines)
        self.current_room = room_name
        if room_name not in self.room_info:
            self.room_info[room_name] = (doors, items, " ".join(lines))
        if room_name not in self.room_connections:
            self.room_connections[room_name] = {d: None for d in doors}

    def gen_room_info(self, lines):
        room_idxs = [i for i, l in enumerate(lines) if l.startswith("==")]
        room_lines = [
            lines[i:j] for i, j in zip(room_idxs, [*room_idxs[1:], len(lines)])
        ]

        for sublines in room_lines:
            try:
                (room_name,) = (
                    re.match(r"== (.*) ==", l).group(1)
                    for l in sublines
                    if l.startswith("==")
                )
            except ValueError:
                print("".join(lines))
                raise
            doors = list(self.gen_doors(sublines))
            items = list(self.gen_items(sublines))
            yield room_name, doors, items

    def gen_doors(self, lines):
        return self._gen_room_info("Doors here lead:", lines)

    def gen_items(self, lines):
        return self._gen_room_info("Items here:", lines)

    def _gen_room_info(self, line_indicator, lines):
        iter_lines = iter(lines)
        while True:
            try:
                l = next(iter_lines)
            except StopIteration:
                return
            if l == line_indicator:
                break
        while True:
            l = next(iter_lines)
            if not l.startswith("- "):
                break
            thing = l[2:]
            yield thing

    def take_all_items(self):
        items_to_avoid = [
            "escape pod",
            "giant electromagnet",
            "photons",
            "infinite loop",
            "molten lava",
        ]
        n, items, t = self.room_info[self.current_room]
        for item in items:
            if item in items_to_avoid:
                continue
            self._run_command(f"take {item}")
        self.room_info[self.current_room] = n, [], t

    def drop_all_items(self):
        current_items = list(self.gen_current_items())
        n, prev_items, t = self.room_info[self.current_room]
        for item in current_items:
            self._run_command(f"drop {item}")
        self.room_info[self.current_room] = n, [*current_items, *prev_items], t

    def pick_up_item(self, item):
        n, prev_items, t = self.room_info[self.current_room]
        assert item in prev_items
        self._run_command(f"take {item}")
        self.room_info[self.current_room] = n, [x for x in prev_items if x != item], t

    def gen_current_items(self):
        lines = self._run_command("inv")
        it = iter(lines)
        line = next(it)
        while line != "Items in your inventory:":
            line = next(it)
        line = next(it)
        while line.startswith("- "):
            yield line[2:]
            line = next(it)

    def validate_command(self, command):
        if command in ["north", "east", "south", "west", "inv"]:
            return
        if command.startswith("take ") or command.startswith("drop "):
            return
        raise ValueError(command)

    def opposite_direction(self, direction):
        if direction == "north":
            return "south"
        elif direction == "south":
            return "north"
        elif direction == "east":
            return "west"
        elif direction == "west":
            return "east"
        raise RuntimeError(direction)


def part1():
    robot = Robot(read_input())
    robot.try_items_with_pressure_sensitive_floor()


if __name__ == "__main__":
    part1()
