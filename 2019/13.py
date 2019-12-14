import json
from collections import deque
from pathlib import Path

from intcode import run_intcode, IntcodeState, InputRequiredError


def part1():
    instructions = [int(x) for x in Path("input/13.txt").read_text().strip().split(",")]
    state = IntcodeState(instructions)
    inputs = deque()
    outputs = deque()

    run_intcode(state, inputs, outputs)

    print(sum(1 for x in list(outputs)[2::3] if x == 2))


class Game:
    WIDTH = 40
    HEIGHT = 25

    def __init__(self, instructions):
        self.state = IntcodeState(instructions)
        self.inputs = deque()
        self.outputs = deque()
        self.screen = [["1" for _ in range(self.WIDTH)] for _ in range(self.HEIGHT)]

        self.score = None
        self.prev_score = None

        self.commands = list()

    def draw(self):
        outputs = list(self.outputs)
        pixels = [outputs[n : n + 3] for n in range(0, len(self.outputs), 3)]

        paddle_pos = None
        ball_pos = None

        for x, y, t in pixels:
            if x == -1:
                self.prev_score = self.score
                self.score = t
                continue
            if t == 0:
                p = " "
            elif t == 1:
                p = "|"
            elif t == 2:
                p = "@"
            elif t == 3:
                p = "-"
                paddle_pos = (x, y)
            elif t == 4:
                p = "*"
                ball_pos = (x, y)
            else:
                raise RuntimeError
            self.screen[y][x] = p
        self.outputs.clear()
        lines = ["".join(l) for l in self.screen]

        lines[2] += f"    Padd pos: {paddle_pos}"
        lines[3] += f"    Ball pos: {ball_pos}"

        if self.score and self.prev_score:
            lines[5] += f"    Score increase: {self.score - self.prev_score}"

        print(f"Score: {self.score}")
        print("\n".join(lines))

    def mem_inspect(self, value):
        mem = [(i, x) for i, x in enumerate(self.state.memory.memory) if x == value]
        print(mem)

    def mem_set(self, addr, value):
        self.state.memory.memory[addr] = value

    def mem_get(self, addr):
        return self.state.memory.memory[addr]

    def read_input(self):
        while True:
            x = input("command: ")
            if x in ["", "s"]:
                self.commands.append(0)
                self.inputs.append(0)
                break
            elif x.startswith("a"):
                for c in x:
                    self.commands.append(-1)
                    self.inputs.append(-1)
                break
            elif x.startswith("d"):
                for c in x:
                    self.commands.append(1)
                    self.inputs.append(1)
                break
            elif x == "bx+":
                self.mem_set(388, self.mem_get(388) + 1)
            elif x.startswith("setx"):
                _, v = x.split(":")
                self.mem_set(388, int(v))
            else:
                print("unknown input, try again")

    def play(self, commands):
        """Play the game by hand"""
        while True:
            try:
                if commands:
                    self.commands = list(commands)
                    self.inputs.extend(commands)
                    commands = None
                run_intcode(self.state, self.inputs, self.outputs)
                self.draw()
                print(f"Game Over. Score: {self.score}")
                return -1
            except InputRequiredError:
                self.draw()
                self.read_input()

    def solve(self):
        """Play the game automatically"""
        paddle_pos, ball_pos = None, None
        while True:
            try:
                run_intcode(self.state, self.inputs, self.outputs)
                self.read_output()
                break
            except InputRequiredError:
                pass
            p, b = self.read_output()
            paddle_pos = p if p is not None else paddle_pos
            ball_pos = b if b is not None else ball_pos

            if paddle_pos == ball_pos:
                self.inputs.append(0)
            elif paddle_pos < ball_pos:
                self.inputs.append(1)
            else:
                self.inputs.append(-1)

        print(f"Final score: {self.score}")

    def read_output(self):
        outputs = list(self.outputs)
        pixels = [outputs[n : n + 3] for n in range(0, len(self.outputs), 3)]

        paddle_pos = None
        ball_pos = None

        for x, _, t in pixels:
            if x == -1:
                self.score = t
                continue
            if t == 3:
                paddle_pos = x
            elif t == 4:
                ball_pos = x
        self.outputs.clear()
        return paddle_pos, ball_pos


def part2_manually_play():
    instructions = [int(x) for x in Path("input/13.txt").read_text().strip().split(",")]
    instructions[0] = 2

    save_path = Path(".13.save")
    num_to_leave = 35

    if save_path.exists():
        saved = json.loads(save_path.read_text().strip())
        best_commands = saved["commands"][:-num_to_leave]
        best_score = saved["score"]
    else:
        best_commands = None
        best_score = 0

    while True:
        game = Game(instructions)
        x = game.play(best_commands)
        if game.score and game.score > best_score:
            best_score = game.score
            best_commands = game.commands[:-num_to_leave]
            save_path.write_text(
                json.dumps({"score": best_score, "commands": best_commands,})
            )

        if x != -1:
            break


def part2():
    instructions = [int(x) for x in Path("input/13.txt").read_text().strip().split(",")]
    instructions[0] = 2
    Game(instructions).solve()


if __name__ == "__main__":
    # part1()
    part2()
