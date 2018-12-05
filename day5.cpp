#include <vector>
#include <iostream>
#include <string>
#include <map>
#include <set>
#include <tuple>
#include <cassert>
#include <cctype>
#include <climits>

std::string read_input() {
    std::string input;
    std::getline(std::cin, input);
    return input;
}

// part1 - fun with pointers
struct Node {
    char val;
    Node *next;
    Node *prev;
    Node(char v, Node *n, Node *p): val(v), next(n), prev(p) {};
};

void CleanUp(Node *start) {
    Node *it = start;
    Node *jt;
    while (it != NULL) {
        jt = it->next;
        delete it;
        it = jt;
    }
}

std::pair<Node*,Node*> BuildNodeList(std::string &input) {
    Node *start;
    Node *prev = NULL;
    int len = 0;
    for (auto &it: input) {
        len++;
        Node *cur = new Node(it, NULL, prev);
        if (len == 1) {
            start = cur;
        } else {
            prev->next = cur;
        }
        prev = cur;
    }
    return std::pair<Node*,Node*>(start, prev);
}

void PrintNodeList(Node *start) {
    Node *it = start;
    while (it != NULL) {
        std::cout << it->val << " ";
        it = it->next;
    }
    std::cout << std::endl;
}

Node *Reduce(Node *start) {
    // Idea: iterate over node list
    //       destroying matching pairs of nodes
    //       this get a bit janky because there are some edge cases to think about
    Node *cur;
    bool isComplete = 0;
    // int idx;
    while (!isComplete) {
        cur = start;
        // idx = 0;
        while (cur != NULL) {
            if (cur->next == NULL) {
                isComplete = 1;
                break;
            }
            char cVal = cur->val;
            char nVal = cur->next->val;
            // std::cout << "curVal: " << cVal << " nVal: " << nVal << std::endl;
            if (char(tolower(cVal)) == char(tolower(nVal)) && cVal != nVal) {
                if (cur == start) {
                    start = cur->next->next;
                    start->prev = NULL;
                } else if (cur->next->next == NULL) {
                    cur->prev->next = NULL;
                } else {
                    cur->prev->next = cur->next->next;
                    cur->next->next->prev = cur->prev;
                }
                // std::cout << "idx at delete: " << idx << std::endl;
                delete cur->next;
                delete cur;
                break;
            }
            cur = cur->next;
            // idx++;
        }
    }
    return start;
}

void part1ComplicatedButWithPointerFun(std::string input) {
    Node *start;
    Node *last;
    std::pair<Node*,Node*> ptrs = BuildNodeList(input);
    start = ptrs.first;
    last = ptrs.second;

    // PrintNodeList(start);
    start = Reduce(start);
    // PrintNodeList(start);

    int len = 0;
    Node *it = start;
    while (it != NULL) {
        len++;
        it = it->next;
    }
    std::cout << "num units: " << len << std::endl;
    CleanUp(start);
}

// part1 - fun with strings
void part1Simple(std::string input) {
    bool isComplete = 0;
    int curPos;
    while (!isComplete) {
        curPos = 0;
        while (curPos < input.size()) {
            if (curPos + 1 == input.size()) {
                isComplete = 1;
                break;
            }
            char cVal = input[curPos];
            char nVal = input[curPos + 1];
            if (char(tolower(cVal)) == char(tolower(nVal)) && cVal != nVal) {
                input.erase(curPos, 2);
                break;
            }
            curPos++;
        }
    }
    std::cout << "num units: " << input.size() << std::endl;
    // std::cout << input << std::endl;
}

// part2
void DeleteCharFromPos(std::string &input, int startPos, char charToRemove) {
    int i = 0;
    while (char(tolower(input[startPos + i])) == charToRemove) {
        i++;
    }
    if (i > 0) {
        input.erase(startPos, i);
    }
}

std::string ReduceWithRemovedChar(std::string input, char removedChar) {
    DeleteCharFromPos(input, 0, removedChar);

    bool isComplete = 0;
    int curPos;
    while (!isComplete) {
        curPos = 0;
        while (curPos < input.size()) {
            DeleteCharFromPos(input, curPos + 1, removedChar);
            if (curPos + 1 == input.size()) {
                isComplete = 1;
                break;
            }
            char cVal = input[curPos];
            char nVal = input[curPos + 1];
            if (char(tolower(cVal)) == char(tolower(nVal)) && cVal != nVal) {
                input.erase(curPos, 2);
                break;
            }
            curPos++;
        }
    }
    return input;
}

void part2(std::string input) {
    std::set<char> units;
    for (auto &it: input) {
        units.insert(char(tolower(it)));
    }

    // try each unit in units, get the one with the smallest reduced string.
    int minLen = INT_MAX;
    for (auto &it: units) {
        std::string reduced = ReduceWithRemovedChar(input, it);
        if (reduced.size() < minLen) {
            minLen = reduced.size();
        }
    }

    std::cout << "min len: " << minLen << std::endl;
}

int main() {
    std::string input = read_input();

    // part1ComplicatedButWithPointerFun(input);
    // part1Simple(input);
    part2(input);
    return 0;
}
