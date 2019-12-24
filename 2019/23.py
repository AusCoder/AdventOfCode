from collections import deque, defaultdict
from pathlib import Path

from intcode import IntcodeState, InputRequiredError, run_intcode


def read_input():
    return [int(x) for x in Path("input/23.txt").read_text().strip().split(",")]


class Nat:
    def __init__(self):
        self.packet = None

    def has_packet(self):
        return bool(self.packet)

    def receive_packet(self, packet):
        self.packet = packet

    def is_network_idle(self, computers):
        return not any(
            c.has_received_packets or c.is_sending_packets for c in computers
        )


class Nic:
    def __init__(self, computers):
        self.computers = {c.address: c for c in computers}
        self.nat = Nat()

    def send_to(self, addr, packet):
        if addr == 255:
            self.nat.receive_packet(packet)
        else:
            self.computers[addr].receive_packet(packet)

    def send_nat_packet(self):
        self.computers[0].receive_packet(self.nat.packet)


class Computer:
    def __init__(self, address, instructions):
        self.address = address
        self.packets = deque()
        self.is_sending_packets = True

        self.state = IntcodeState(instructions)
        self.inputs = deque()
        self.outputs = deque()
        self.inputs.append(self.address)
        try:
            run_intcode(self.state, self.inputs, self.outputs)
        except InputRequiredError:
            pass

    @property
    def has_received_packets(self):
        return bool(self.packets)

    def run(self, nic):
        if not self.packets:
            self.inputs.append(-1)
        else:
            for x, y in self.packets:
                self.inputs.append(x)
                self.inputs.append(y)
        self.packets.clear()

        try:
            run_intcode(self.state, self.inputs, self.outputs)
        except InputRequiredError:
            pass
        self.is_sending_packets = bool(self.outputs)

        for addr, packet in self._gen_packets():
            nic.send_to(addr, packet)

    def receive_packet(self, packet):
        self.packets.append(packet)

    def _gen_packets(self):
        while True:
            try:
                addr = self.outputs.popleft()
            except IndexError:
                break
            yield addr, (self.outputs.popleft(), self.outputs.popleft())


def part1():
    instructions = read_input()

    computers = [Computer(i, instructions) for i in range(50)]
    nic = Nic(computers)
    while not nic.nat.has_packet():
        for c in computers:
            c.run(nic)

    print(nic.nat.packet)


def part2():
    instructions = read_input()

    computers = deque(Computer(i, instructions) for i in range(50))
    nic = Nic(computers)

    prev_nat_packet_y = None
    while True:
        while not nic.nat.is_network_idle(computers):
            c = computers[0]
            c.run(nic)
            computers.rotate(-1)
        _, y = nic.nat.packet
        if y == prev_nat_packet_y:
            break
        prev_nat_packet_y = y
        nic.send_nat_packet()

    print(y)


if __name__ == "__main__":
    # part1()
    part2()
