#include <vector>
#include <iostream>
#include <sstream>
#include <string>
#include <map>
#include <tuple>
#include <cassert>
#include <regex>
#include <iomanip>
#include <ctime>
#include <chrono>
#include <algorithm>
#include <numeric>

class Event {
    public:
        std::time_t time;
        int min;
        std::string timeStr;
        std::string description;
    Event(std::time_t t, int m, std::string ts, std::string d) {
        time = t;
        min = m;
        timeStr = ts;
        description = d;
    };
    friend bool operator<(Event a, Event b) {
        return a.time < b.time;
    }
};

std::vector<Event> read_input() {
    std::vector<Event> vals;
    std::string input;
    std::cmatch groups;
    std::cmatch groups2;
    std::time_t t;
    std::regex txt_regex("^\\[(\\d+-\\d+-\\d+\\ \\d+:\\d+)\\]\\ (.*)$");

    while (std::getline(std::cin, input)) {
        bool res = std::regex_match(input.c_str(), groups, txt_regex);
        assert(res);
        std::tm tm = {0};
        std::stringstream ss(groups[1]);
        ss >> std::get_time(&tm, "%Y-%m-%d %H:%M");
        tm.tm_year = 100;
        t = mktime(&tm);

        std::string s = std::string(groups[1]);

        Event event = Event(t, tm.tm_min, s, groups[2]);
        vals.push_back(event);
    }
    return vals;
}

// part1
int ExtractId(const Event &event) {
    std::cmatch groups;
    std::regex txt_regex("^Guard #(\\d+) begins shift$");
    bool res = std::regex_match(event.description.c_str(), groups, txt_regex);
    assert(res);
    return std::stoi(groups[1]);
}

std::pair<int,int> maxWithIdx(std::vector<int> &vals) {
    int maxVal = -1;
    int maxIdx;
    for (int idx=0; idx<vals.size(); idx++) {
        if (vals[idx] > maxVal) {
            maxVal = vals[idx];
            maxIdx = idx;
        }
    }
    return std::pair<int,int>(maxVal, maxIdx);
}

std::map<int,std::vector<int>> BuildMinuteCountsMap(const std::vector<Event> &inputs) {
    std::map<int,std::vector<int>> countsMap;
    int curGuardId;
    bool isAwake = 1;
    int fellAsleepAt;
    int wokeUpAt;

    for (auto &it: inputs) {
        // std::cout << it.timeStr << "   " << it.description << std::endl;
        if (it.description.compare(0, 5, "Guard") == 0) {
            curGuardId = ExtractId(it);
            auto found = countsMap.find(curGuardId);
            if (found == countsMap.end()) {
                countsMap[curGuardId] = std::vector<int>(60, 0);
            }
        } else if (it.description.compare(0, 4, "fall") == 0) {
            assert(isAwake);
            isAwake = 0;
            fellAsleepAt = it.min;
        } else if (it.description.compare(0, 4, "wake") == 0) {
            assert(!isAwake);
            isAwake = 1;
            wokeUpAt = it.min;
            for (int i=fellAsleepAt; i<wokeUpAt; i++) {
                countsMap.at(curGuardId)[i]++;
            }
        } else {
            assert(0);
        }
    }
    return countsMap;
}

void part1(const std::vector<Event> &inputs) {
    std::map<int,std::vector<int>> countsMap = BuildMinuteCountsMap(inputs);

    int maxAsleepId;
    int maxAsleep = -1;
    for (auto &it: countsMap) {
        int totalAsleep = std::accumulate(it.second.begin(), it.second.end(), 0);
        if (totalAsleep >  maxAsleep) {
            maxAsleepId = it.first;
            maxAsleep = totalAsleep;
        }
    }
    std::pair<int,int> maxAndIdx = maxWithIdx(countsMap[maxAsleepId]);
    std::cout << "max asleep id: " << maxAsleepId << "     id * min: " << (maxAsleepId * maxAndIdx.second) << std::endl;
}

// part2
void part2(const std::vector<Event> inputs) {
    std::map<int,std::vector<int>> countsMap = BuildMinuteCountsMap(inputs);

    int maxAsleepMinId;
    int maxAsleepMin;
    int maxAsleepMinVal = -1;
    for (auto &it: countsMap) {
        std::pair<int,int> maxAndIdx = maxWithIdx(it.second);
        if (maxAndIdx.first > maxAsleepMinVal) {
            maxAsleepMinId = it.first;
            maxAsleepMin = maxAndIdx.second;
            maxAsleepMinVal = maxAndIdx.first;
        }
    }

    std::cout << "max asleep min id: " << maxAsleepMinId << "     id * min: " << (maxAsleepMinId * maxAsleepMin) << std::endl;
}

int main() {
    std::vector<Event> inputs = read_input();

    std::sort(inputs.begin(), inputs.end());

    part1(inputs);
    part2(inputs);
    return 0;
}
