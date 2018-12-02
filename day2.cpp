#include <cstdio>
#include <vector>
#include <iostream>
#include <string>
#include <map>
#include <tuple>
#include <cassert>

std::vector<std::string> read_input() {
    std::vector<std::string> vals;
    std::string input;
    while (std::cin >> input) {
        vals.push_back(input);
    }
    return vals;
}

// part 1
std::pair<int, int> classify_string(const std::string &input) {
    std::map<char, int> counts;
    for (auto it=input.begin(); it!=input.end(); it++) {
        auto found = counts.find(*it);
        if (found == counts.end()) {
            counts[*it] = 1;
        } else {
            counts.at(*it)++;
        }
    }

    int twos = 0;
    int threes = 0;
    for (auto it=counts.begin(); it!=counts.end(); it++) {
        if (it->second == 2) {
            twos = 1;
        }
        if (it -> second == 3) {
            threes = 1;
        }
    }
    return std::pair<int,int> (twos, threes);
}

void part1(std::vector<std::string> inputs) {
    int twos = 0;
    int threes = 0;
    for (auto it = inputs.begin(); it != inputs.end(); it++) {
        std::pair<int,int> p = classify_string(*it);
        twos += p.first;
        threes += p.second;
    }
    std::cout << "checksum: " << twos * threes << std::endl;
}

// part2
bool is_id_match(const std::string &id1, const std::string &id2) {
    int num_diff = 0;
    for (int n=0; n<id1.length(); n++) {
        if (id1[n] != id2[n]) {
            num_diff++;
        }
        if (num_diff > 2) {
            break;
        }
    }
    assert(num_diff != 0);
    return num_diff == 1;
}

std::string filter_matching_ids(const std::string &id1, const std::string &id2) {
    std::string ret;
    for (int n=0; n<id1.length(); n++) {
        if (id1[n] != id2[n]) {
            continue;
        }
        ret += id1[n];
    }
    return ret;
}

void part2(std::vector<std::string> inputs) {
    std::string id1;
    std::string id2;
    bool should_break = 0;
    for (auto it = inputs.begin(); it != inputs.end(); it++) {
        for (auto jt = it+1; jt != inputs.end(); jt++) {
            bool is_match = is_id_match(*it, *jt);
            if (is_match) {
                id1 = *it;
                id2 = *jt;
                should_break = 1;
                break;
            }
        }
        if (should_break) {
            break;
        }
    }
    std::cout << "matching ids: " << id1 << ", " << id2 << std::endl;
    std::cout << "filtered id: " << filter_matching_ids(id1, id2) << std::endl;
}

int main() {
    std::vector<std::string> inputs = read_input();
    part1(inputs);
    part2(inputs);
    return 0;
}
