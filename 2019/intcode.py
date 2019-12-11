from typing import List, Deque


class InputRequiredError(Exception):
    pass


class IntcodeMemory:
    def __init__(self, instructions: List[int]) -> None:
        self.memory = list(instructions)

    def __getitem__(self, idx: int) -> int:
        self._check_and_grow(idx)
        return self.memory[idx]

    def __setitem__(self, idx: int, value: int) -> None:
        self._check_and_grow(idx)
        self.memory[idx] = value

    def _check_and_grow(self, idx: int) -> None:
        if idx < 0:
            raise ValueError
        if idx >= len(self.memory):
            self.memory += [0] * (idx + 1 - len(self.memory))


m = IntcodeMemory([1, 2, 3])


class IntcodeState:
    def __init__(self, instructions: List[int]) -> None:
        self.memory = IntcodeMemory(instructions)
        self.instruction_pointer = 0
        self.relative_base = 0
        self.is_halted = False

    def __repr__(self):
        return f"{self.__class__.__name__}(ip={self.instruction_pointer})"


def run_intcode(state: IntcodeState, inpts: Deque[int], outputs: Deque[int]) -> None:
    memory = state.memory
    while True:
        op_code, modes, num_args = parse_instruction(memory[state.instruction_pointer])
        did_jump = False
        if op_code == 99:
            state.is_halted = True
            break

        if op_code in [1, 2, 7, 8]:
            arg1 = arg_value(state, modes, 1)
            arg2 = arg_value(state, modes, 2)
            assert parameter_mode(modes, 3) in [0, 2]
            res_addr = arg_addr(state, modes, 3)

            if op_code == 1:
                memory[res_addr] = arg1 + arg2
            elif op_code == 2:
                memory[res_addr] = arg1 * arg2
            elif op_code == 7:
                memory[res_addr] = 1 if arg1 < arg2 else 0
            elif op_code == 8:
                memory[res_addr] = 1 if arg1 == arg2 else 0
            else:
                raise RuntimeError
        elif op_code == 3:
            assert parameter_mode(modes, 1) in [0, 2]
            res_addr = arg_addr(state, modes, 1)
            try:
                memory[res_addr] = inpts.popleft()
            except IndexError:
                raise InputRequiredError
        elif op_code == 4:
            arg1 = arg_value(state, modes, 1)
            outputs.append(arg1)
        elif op_code in [5, 6]:
            arg1 = arg_value(state, modes, 1)
            arg2 = arg_value(state, modes, 2)
            if op_code == 5 and arg1 != 0:
                state.instruction_pointer = arg2
                did_jump = True
            elif op_code == 6 and arg1 == 0:
                state.instruction_pointer = arg2
                did_jump = True
        elif op_code == 9:
            arg1 = arg_value(state, modes, 1)
            state.relative_base += arg1
        else:
            raise RuntimeError(op_code, modes, num_args)

        if not did_jump:
            state.instruction_pointer += 1 + num_args


def parse_instruction(instruction):
    op_code = instruction % 100
    parameter_modes = instruction // 100
    if op_code in [1, 2, 7, 8]:
        num_args = 3
    elif op_code in [3, 4, 9]:
        num_args = 1
    elif op_code in [5, 6]:
        num_args = 2
    elif op_code in [99]:
        num_args = 0
    else:
        raise RuntimeError(instruction)
    return op_code, parameter_modes, num_args


def parameter_mode(modes, posn):
    return (modes // (10 ** (posn - 1))) % 10


def arg_addr(state, modes, arg_num):
    mode = parameter_mode(modes, arg_num)
    if mode == 0:
        return state.memory[state.instruction_pointer + arg_num]
    elif mode == 1:
        return state.instruction_pointer + arg_num
    elif mode == 2:
        return state.relative_base + state.memory[state.instruction_pointer + arg_num]
    raise RuntimeError


def arg_value(state, modes, arg_num):
    return state.memory[arg_addr(state, modes, arg_num)]
