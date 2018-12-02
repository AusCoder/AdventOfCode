#include <cmath>
#include <cstdio>
#include <vector>
#include <set>
#include <iostream>
#include <algorithm>

std::vector<int> read_input() {
    std::vector<int> vals;
    int input;
    while (std::cin >> input) {
        vals.push_back(input);
    }
    return vals;
}

void part1(std::vector<int> inputs) {
    int frequency = 0;

    for (auto it = inputs.begin(); it != inputs.end(); it++) {
        frequency += *it;
    }
    std::cout << "frequency: " << frequency << std::endl;
}

void part2(std::vector<int> inputs) {
    int frequency = 0;
    int should_stop = 0;
    std::set<int> seen_frequencies {frequency};

    while (1) {
        for (auto it = inputs.begin(); it != inputs.end(); it++) {
            frequency += *it;
            auto find_it = seen_frequencies.find(frequency);
            if (find_it != seen_frequencies.end()) {
                should_stop = 1;
                break;
            } else {
                seen_frequencies.insert(frequency);
            }
        }
        if (should_stop) {
            break;
        }
    }

    std::cout << "first repeat frequency: " << frequency << std::endl;
}

int main() {
    std::vector<int> inputs = read_input();
    part1(inputs);
    part2(inputs);
    return 0;
}
