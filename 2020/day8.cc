#include "bits-and-bobs.hh"
#include <unordered_set>

using namespace std;

struct Instruction {
  string name;
  int argument;
};

struct Result {
  bool isInfiniteLoop;
  int accumulator;
};

Instruction parseInstruction(const string &line) {
  std::regex r("([a-z]+)\\s([+\\-])(\\d+)");
  std::smatch m;
  std::regex_match(line, m, r);
  assert(!m.empty());
  string name{m.str(1)};
  string sign{m.str(2)};
  int argument{std::stoi(m.str(3))};
  if (sign == "-") {
    argument = -argument;
  }
  return {name, argument};
}

Result runInstructions(const vector<Instruction> &instructions) {
  int accumulator = 0;
  bool isInfiniteLoop = true;
  int instructionPointer = 0;
  unordered_set<int> seenIdxs;
  while (seenIdxs.find(instructionPointer) == seenIdxs.end()) {
    if (static_cast<std::size_t>(instructionPointer) >= instructions.size()) {
      isInfiniteLoop = false;
      break;
    }
    const Instruction &instruction = instructions.at(instructionPointer);
    seenIdxs.insert(instructionPointer);
    if (instruction.name == "nop") {
      instructionPointer++;
    } else if (instruction.name == "acc") {
      accumulator += instruction.argument;
      instructionPointer++;
    } else if (instruction.name == "jmp") {
      instructionPointer += instruction.argument;
    } else {
      throw std::runtime_error(string("unknown instruction ") +
                               instruction.name);
    }
  }
  return {isInfiniteLoop, accumulator};
}

void part1(const vector<Instruction> &instructions) {
  auto result = runInstructions(instructions);
  print(result.accumulator);
}

void part2(const vector<Instruction> &instructions) {
  for (std::size_t idx = 0; idx < instructions.size(); idx++) {
    vector<Instruction> instructionsCopy{instructions.begin(),
                                         instructions.end()};
    const auto &instruction = instructionsCopy.at(idx);
    bool didChange = false;
    if (instruction.name == "nop") {
      instructionsCopy.at(idx) = {"jmp", instruction.argument};
      didChange = true;
    } else if (instruction.name == "jmp") {
      instructionsCopy.at(idx) = {"nop", instruction.argument};
      didChange = true;
    }
    if (didChange) {
      auto result = runInstructions(instructionsCopy);
      if (!result.isInfiniteLoop) {
        print(result.accumulator);
        break;
      }
    }
  }
}

int main() {
  auto lines = readLinesFromFile("input/day8.txt");
  vector<Instruction> instructions(lines.size());
  transform(lines.cbegin(), lines.cend(), instructions.begin(),
            parseInstruction);
  part1(instructions);
  part2(instructions);
}
