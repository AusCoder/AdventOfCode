from collections import deque
from enum import Enum
from pathlib import Path

from intcode import run_intcode, IntcodeState, InputRequiredError


class Direction(Enum):
    UP = 0
    DOWN = 1
    LEFT = 2
    RIGHT = 3


class TurnInstruction(Enum):
    TURN_LEFT = 0
    TURN_RIGHT = 1

    @staticmethod
    def from_output(output):
        if output == 0:
            return TurnInstruction.TURN_LEFT
        elif output == 1:
            return TurnInstruction.TURN_RIGHT
        raise RuntimeError(f"unknown output: {output}")


class Robot:
    def __init__(self, x: int, y: int, direction: Direction) -> None:
        self.x = x
        self.y = y
        self.direction = direction

    def render_direction(self):
        if self.direction == Direction.UP:
            return "^"
        elif self.direction == Direction.DOWN:
            return "v"
        elif self.direction == Direction.LEFT:
            return "<"
        elif self.direction == Direction.RIGHT:
            return ">"
        else:
            raise RuntimeError

    def move_forward(self):
        if self.direction == Direction.UP:
            self.y -= 1
        elif self.direction == Direction.DOWN:
            self.y += 1
        elif self.direction == Direction.LEFT:
            self.x -= 1
        elif self.direction == Direction.RIGHT:
            self.x += 1
        else:
            raise RuntimeError

    def turn(self, turn_instruction: TurnInstruction):
        if turn_instruction == TurnInstruction.TURN_LEFT:
            if self.direction == Direction.UP:
                self.direction = Direction.LEFT
            elif self.direction == Direction.DOWN:
                self.direction = Direction.RIGHT
            elif self.direction == Direction.LEFT:
                self.direction = Direction.DOWN
            elif self.direction == Direction.RIGHT:
                self.direction = Direction.UP
            else:
                raise RuntimeError
        elif turn_instruction == TurnInstruction.TURN_RIGHT:
            if self.direction == Direction.UP:
                self.direction = Direction.RIGHT
            elif self.direction == Direction.DOWN:
                self.direction = Direction.LEFT
            elif self.direction == Direction.LEFT:
                self.direction = Direction.UP
            elif self.direction == Direction.RIGHT:
                self.direction = Direction.DOWN
            else:
                raise RuntimeError
        else:
            raise RuntimeError

    def read_panel(self, panels: dict) -> int:
        return panels.get((self.x, self.y), 0)

    def paint_panel(self, panels: dict, color: int) -> None:
        panels[(self.x, self.y)] = color


class State:
    def __init__(self, instructions, initial_color=0) -> None:
        self.robot = Robot(0, 0, Direction.UP)
        self.panels = {(0, 0): initial_color}
        self.intcode_state = IntcodeState(instructions)
        self.intcode_inputs = deque()
        self.intcode_outputs = deque()

    def __repr__(self):
        x_range = [x for x, _ in self.panels]
        y_range = [y for _, y in self.panels]
        min_x = min(x_range, default=0) - 2
        min_y = min(y_range, default=0) - 2
        max_x = max(x_range, default=0) + 3
        max_y = max(y_range, default=0) + 3

        grid = [["."] * (max_x - min_x) for _ in range(max_y - min_y)]
        for (x, y), color in self.panels.items():
            if color == 1:
                grid[y - min_y][x - min_x] = "#"
        grid[self.robot.y - min_y][self.robot.x - min_x] = self.robot.render_direction()
        grid = "\n".join("".join(l) for l in grid)
        return f"Panels:\n{grid}"

    def run_round(self):
        current_color = self.robot.read_panel(self.panels)
        self.intcode_inputs.append(current_color)
        try:
            run_intcode(self.intcode_state, self.intcode_inputs, self.intcode_outputs)
            return
        except InputRequiredError:
            pass
        color_to_paint, _turn_instr = self.intcode_outputs
        turn_instruction = TurnInstruction.from_output(_turn_instr)
        self.intcode_outputs.clear()

        self.robot.paint_panel(self.panels, color_to_paint)
        self.robot.turn(turn_instruction)
        self.robot.move_forward()

    def run(self):
        while not self.intcode_state.is_halted:
            self.run_round()


def part1():
    instructions = [int(x) for x in Path("input/11.txt").read_text().strip().split(",")]
    state = State(instructions)
    state.run()
    print(state)
    print(len(state.panels))


def part2():
    instructions = [int(x) for x in Path("input/11.txt").read_text().strip().split(",")]
    state = State(instructions, 1)
    state.run()
    print(state)
    print(len(state.panels))


if __name__ == "__main__":
    part1()
    part2()
