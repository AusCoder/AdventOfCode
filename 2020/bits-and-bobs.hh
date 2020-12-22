#ifndef _BITS_AND_BOBS_HH
#define _BITS_AND_BOBS_HH

#include <algorithm>
#include <array>
#include <cassert>
#include <deque>
#include <fstream>
#include <functional>
#include <iostream>
#include <iterator>
#include <map>
#include <memory>
#include <numeric>
#include <regex>
#include <sstream>
#include <unordered_map>
#include <vector>

template <typename T> void printContainer(const T &con) {
  for (const auto &x : con) {
    std::cout << x << " ";
  }
  std::cout << "\n";
}

template <typename T>
void printMatrix(const std::vector<std::vector<T>> &matrix) {
  for (const auto &vec : matrix) {
    printContainer(vec);
  }
}

template <typename T> void printMap(const T &map) {
  for (const auto &item : map) {
    std::cout << item.first << " -> " << item.second << "  ";
  }
  std::cout << "\n";
}

template <typename T> void print(const T &thing) { std::cout << thing << "\n"; }

template <typename T, typename S> void printPair(const std::pair<T, S> &p) {
  std::cout << "(" << p.first << ", " << p.second << ")\n";
}

template <typename T>
std::vector<T> getColumn(int colIdx,
                         const std::vector<std::vector<T>> &matrix) {
  std::vector<T> col;
  std::transform(matrix.cbegin(), matrix.cend(), std::back_inserter(col),
                 [=](auto row) { return row.at(colIdx); });
  return col;
}

/*
  Split a string using a regex string.
  This works well for commas.
*/
std::vector<std::string> splitString(const std::string &s,
                                     const std::string &regexStr) {
  std::vector<std::string> parts;
  std::regex sepRe(regexStr);
  std::copy(std::sregex_token_iterator(s.cbegin(), s.cend(), sepRe, -1),
            std::sregex_token_iterator(), std::back_inserter(parts));
  return parts;
}

bool startsWith(const std::string &s, const std::string &prefix) {
  return s.substr(0, prefix.size()) == prefix;
}

std::string trimWhitespace(const std::string &s) {
  std::regex contentRe("\\s*(.*)\\s*");
  std::smatch contentMatch;
  std::regex_match(s, contentMatch, contentRe);
  assert(!contentMatch.empty());
  return contentMatch.str(1);
}

/*
  Read lines from a file, possibly without reading
  final empty line.
*/
std::vector<std::string> readLinesFromFile(const std::string &filePath,
                                           bool dropFinalEmptyLine = true) {
  std::ifstream iStream{filePath};
  if (!iStream.is_open()) {
    throw std::runtime_error(std::string("Failed to open ") + filePath);
  }

  std::vector<std::string> lines;
  std::string line;
  for (;;) {
    if (iStream.eof()) {
      break;
    }
    std::getline(iStream, line);
    lines.push_back(line);
  }
  if (dropFinalEmptyLine && (lines.at(lines.size() - 1).empty())) {
    lines.erase(lines.cend() - 1);
  }
  return lines;
}

std::vector<std::string> readLinesUntilEmptyLine(std::istream &iStream) {
  std::vector<std::string> lines;
  std::string line;
  for (;;) {
    std::getline(iStream, line);
    if (line.size() == 0) {
      break;
    }
    lines.push_back(line);
  }
  return lines;
}

/*
  Parse matrix from string with space separators
*/
template <typename T, typename F>
std::vector<T> parseVector(const std::string &line, F parseFn) {
  std::istringstream iss(line);
  auto startIter = std::istream_iterator<std::string>(iss);
  auto endIter = std::istream_iterator<std::string>();
  std::vector<std::string> strElems(startIter, endIter);
  std::vector<T> elems;
  std::transform(strElems.cbegin(), strElems.cend(), std::back_inserter(elems),
                 parseFn);
  return elems;
}

std::vector<std::string> parseVectorStr(const std::string &line) {
  auto parseFn = [](const std::string &x) { return x; };
  return parseVector<std::string, decltype(parseFn)>(line, parseFn);
}

std::vector<int> parseVectorInt(const std::string &line) {
  auto parseFn = [](const std::string &s) { return std::stoi(s); };
  return parseVector<int, decltype(parseFn)>(line, parseFn);
}

/*
  Parse matix from vector of strings with space separators
*/
template <typename T, typename F>
std::vector<std::vector<T>> parseMatrix(const std::vector<std::string> &input,
                                        F parseFn) {
  std::vector<std::vector<T>> matrix;
  std::transform(input.cbegin(), input.cend(), std::back_inserter(matrix),
                 [=](const auto &l) { return parseVector<T, F>(l, parseFn); });
  return matrix;
}

std::vector<std::vector<std::string>>
parseMatrixStr(const std::vector<std::string> &lines) {
  auto parseFn = [](std::string x) { return x; };
  return parseMatrix<std::string, decltype(parseFn)>(lines, parseFn);
}

std::vector<std::vector<char>>
parseMatrixChar(const std::vector<std::string> &lines) {
  std::vector<std::vector<char>> matrix(lines.size());
  std::transform(lines.cbegin(), lines.cend(), matrix.begin(),
                 [](const auto &s) {
                   std::vector<char> v{s.begin(), s.end()};
                   return v;
                 });
  return matrix;
}

std::vector<std::vector<int>>
parseMatrixInt(const std::vector<std::string> &lines) {
  auto parseFn = [](std::string x) { return std::stoi(x); };
  return parseMatrix<int, decltype(parseFn)>(lines, parseFn);
}

std::vector<std::vector<long>>
parseMatrixLong(const std::vector<std::string> &lines) {
  auto parseFn = [](std::string x) { return std::stol(x); };
  return parseMatrix<long, decltype(parseFn)>(lines, parseFn);
}

std::vector<std::vector<double>>
parseMatrixDouble(const std::vector<std::string> &lines) {
  auto parseFn = [](std::string x) { return std::stod(x); };
  return parseMatrix<double, decltype(parseFn)>(lines, parseFn);
}

bool isNumber(const std::string &s) {
  return std::all_of(s.cbegin(), s.cend(),
                     [](auto c) { return std::isdigit(c); });
}

long calculateLCM(long x, long y) {
  long s = 1;
  long t = 1;
  for (;;) {
    long sum = x * s - y * t;
    if (sum == 0) {
      return x * s;
    } else if (sum < 0) {
      s++;
    } else {
      t++;
    }
  }
}

long calculateLCM(const std::vector<int> &nums) {
  if (nums.size() <= 1) {
    throw std::invalid_argument("lcm requires at least 2 arguments");
  }
  long lcm = nums.at(0);
  for (size_t idx = 1; idx < nums.size(); idx++) {
    lcm = calculateLCM(lcm, nums.at(idx));
  }
  return lcm;
}

/*
  Returns valid adjacent positions to (x, y) in 2d grid
  of size (width, height)
*/
std::vector<std::pair<int, int>> adjacentPositions2D(int x, int y, int width,
                                                     int height) {
  std::vector<std::pair<int, int>> adj;
  for (int dx = -1; dx < 2; dx++) {
    for (int dy = -1; dy < 2; dy++) {
      adj.push_back(std::make_pair(x + dx, y + dy));
    }
  }
  auto isValidPoint = [&](const auto &pts) {
    return (!((pts.first == x) && (pts.second == y))) && (pts.first >= 0) &&
           (pts.first < width) && (pts.second >= 0) && (pts.second < height);
  };
  std::vector<std::pair<int, int>> validAdj;
  std::copy_if(adj.cbegin(), adj.cend(), std::back_inserter(validAdj),
               isValidPoint);
  return validAdj;
}

/*
  A couple notes on bit shifts:
  x | (1 << i)    sets bit i to 1
  x & ~(1 << i)   sets bit i to 0
*/

#endif
