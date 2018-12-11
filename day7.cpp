#include <vector>
#include <map>
#include <set>
#include <regex>
#include <string>
#include <iostream>
#include <memory>

class Edge;

class Node {
    public:
        char label;
        int indegree;
        int timeTillDone;
        std::vector<Node*> edgeEnds;

        Node(char c) {
            label = c;
            indegree = 0;
            timeTillDone = label - 'A' + 61;
            edgeEnds = std::vector<Node*>();
        };
};

class Edge {
    public:
        const Node &start;
        const Node &end;
        Edge(const Node &s, const Node &e): start(s), end(e) {};
};

class Dag {
    public:
        std::map<char,Node*> nodeMap;
        std::set<char> startNodes;
};

std::vector<std::string> read_input() {
    std::vector<std::string> lines;
    std::string input;
    while (std::getline(std::cin, input)) {
        lines.push_back(input);
    }
    return lines;
}

Dag BuildDag(const std::vector<std::string> &input) {
    std::map<char,Node*> nodes;
    std::set<char> startNodes;
    std::set<char> nonStartNodes;

    char srcChar, dstChar;
    std::cmatch groups;
    std::string match;
    std::regex txt_regex("^Step (.) must be finished before step (.) can begin\\.$");

    for (auto &it: input) {
        bool res = std::regex_match(it.c_str(), groups, txt_regex);
        assert(res);
        match = std::string(groups[1]);
        assert(match.size() == 1);
        srcChar = match[0];
        match = std::string(groups[2]);
        assert(match.size() == 1);
        dstChar = match[0];

        if (nodes.find(srcChar) == nodes.end()) {
            nodes.emplace(srcChar, new Node(srcChar));
        }
        if (nodes.find(dstChar) == nodes.end()) {
            nodes.emplace(dstChar, new Node(dstChar));
        }
        nodes.at(srcChar)->edgeEnds.push_back(nodes.at(dstChar));
        nodes.at(dstChar)->indegree++;

        // add src node to startNodes if it is not a nonStartNode
        auto found = nonStartNodes.find(srcChar);
        if (found == nonStartNodes.end()) {
            startNodes.insert(srcChar);
        }
        // add dst node to nonStartNodes and remove from startNodes, if present
        nonStartNodes.insert(dstChar);
        found = startNodes.find(dstChar);
        if (found != startNodes.end()) {
            startNodes.erase(found);
        }
    }

    Dag ret;
    ret.nodeMap = nodes;
    ret.startNodes = startNodes;
    return ret;
}

// part1
char ClosestToA(const std::set<char> &startNodes) {
    char out = 'Z';
    for (auto &it: startNodes) {
        if (it < out) {
            out = it;
        }
    }
    return out;
}

void part1(std::map<char,Node*> &input, std::set<char> startNodes) {
    std::cout << "node order: ";

    while (!startNodes.empty()) {
        char curChar = ClosestToA(startNodes);
        std::cout << curChar;
        auto it = startNodes.find(curChar);
        if (it != startNodes.end()) {
            startNodes.erase(it);
        }

        auto found = input.find(curChar);
        if (found != input.end()) {
            for (auto &jt: input.at(curChar)->edgeEnds) {
                jt->indegree--;
                if (jt->indegree == 0) {
                    startNodes.insert(jt->label);
                }
            }
        }
    }
    std::cout << std::endl;
}

// part2
void part2(std::map<char,Node*> &input, std::set<char> startNodes) {
    int sec = 0;
    int numWorkers = 5;
    std::vector<char> runningTasks = {};

    // add first tasks
    while (runningTasks.size() < numWorkers && !startNodes.empty()) {
        char curChar = ClosestToA(startNodes);
        auto found = startNodes.find(curChar);
        assert(found != startNodes.end());
        startNodes.erase(found);
        runningTasks.push_back(curChar);
    }

    while (!startNodes.empty() || !runningTasks.empty()) {
        // time to next done task
        int minTTD = INT_MAX;
        for (auto &it: runningTasks) {
            if (input.at(it)->timeTillDone < minTTD) {
                minTTD = input.at(it)->timeTillDone;
            }
        }

// debug
        std::cout << sec << " ";
        // std::cout << "numWorkers: " << numWorkers << " ";
        // std::cout << "startNodes: ";
        // for (auto &it: startNodes) {
        //     std::cout << it << " ";
        // }
        std::cout << "runningTasks: ";
        for (auto &it: runningTasks) {
            std::cout << it << " ttd: " << input.at(it)->timeTillDone << " ";
        }
        std::cout << std::endl;
        // if (sec > 200) {
        //     break;
        // }
        // std::cout << minTTD;

        // count down any running tasks
        // for each runningTask,
        //   timeTillDone--,
        //   if timeTillDone == 0,
        //     remove it,
        //     incr numWorkers,
        //     for each edgeEnd,
        //       indegree of edgeEnd--,
        //       if indegree == 0,
        //         add to startNodes
        for (auto it=runningTasks.begin(); it!=runningTasks.end(); ) {
            input.at(*it)->timeTillDone -= minTTD;
            if (input.at(*it)->timeTillDone == 0) {
                for (auto &jt: input.at(*it)->edgeEnds) {
                    jt->indegree--;
                    if (jt->indegree == 0) {
                        startNodes.insert(jt->label);
                    }
                }
                it = runningTasks.erase(it);
            } else {
                it++;
            }
        }

        // add tasks that are now done
        // while numWorkers > 0 and !startNode.empty(),
        //   numWorkers--,
        //   grab ClosestToA,
        //   remove from startNodes,
        //   add to runningTasks
        while (runningTasks.size() < numWorkers && !startNodes.empty()) {
            char curChar = ClosestToA(startNodes);
            auto found = startNodes.find(curChar);
            assert(found != startNodes.end());
            startNodes.erase(found);
            runningTasks.push_back(curChar);
        }

        sec += minTTD;
    }
    std::cout << std::endl;
    std::cout << "time taken: " << sec << std::endl;
}

int main() {
    std::vector<std::string> lines = read_input();
// debug
    // for (auto &it: input.nodeMap) {
    //     std::cout << it.second->label << " indegree: " << it.second->indegree << std::endl;
    // }
    Dag dag = BuildDag(lines);
    part1(dag.nodeMap, dag.startNodes);

    dag = BuildDag(lines);
// debug
    // for (auto &it: dag.startNodes) {
    //     std::cout << it;
    // }
    // std::cout << std::endl;
    part2(dag.nodeMap, dag.startNodes);

    return 0;
}
