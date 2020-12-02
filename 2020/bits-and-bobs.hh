#ifndef _BITS_AND_BOBS_HH
#define _BITS_AND_BOBS_HH

#include <algorithm>
#include <array>
#include <cassert>
#include <fstream>
#include <functional>
#include <iostream>
#include <iterator>
#include <map>
#include <numeric>
#include <regex>
#include <sstream>
#include <unordered_map>
#include <vector>

template <typename T> void printVector(const std::vector<T> &vec) {
  for (auto &x : vec) {
    std::cout << x << " ";
  }
  std::cout << "\n";
}

template <typename T>
void printMatrix(const std::vector<std::vector<T>> &matrix) {
  for (auto &vec : matrix) {
    printVector(vec);
  }
}

template <typename T>
std::vector<T> pickColumn(const std::vector<std::vector<T>> &matrix,
                          int colIdx) {
  std::vector<T> col;
  std::transform(matrix.cbegin(), matrix.cend(), std::back_inserter(col),
                 [=](auto row) { return row.at(colIdx); });
  return col;
}

std::vector<std::string> readLines(std::istream &iStream) {
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
  Reads a matrix of elements separated by spaces
*/
template <typename T, typename F>
std::vector<std::vector<T>> readMatrix(std::istream &iStream, F parseFn) {
  std::vector<std::vector<T>> parts;
  std::string line;

  for (;;) {
    std::getline(iStream, line);
    if (line.size() == 0) {
      break;
    }
    std::istringstream iss(line);

    auto startIter = std::istream_iterator<std::string>(iss);
    auto endIter = std::istream_iterator<std::string>();
    std::vector<std::string> strElems(startIter, endIter);
    std::vector<T> elems;
    std::transform(strElems.begin(), strElems.end(), std::back_inserter(elems),
                   parseFn);

    parts.push_back(std::move(elems));
  }
  return parts;
}

std::vector<std::vector<std::string>> readMatrixStr(std::istream &iStream) {
  auto parseFn = [](std::string x) { return x; };
  return readMatrix<std::string, decltype(parseFn)>(iStream, parseFn);
}

std::vector<std::vector<int>> readMatrixInt(std::istream &iStream) {
  auto parseFn = [](std::string x) { return std::stoi(x); };
  return readMatrix<int, decltype(parseFn)>(iStream, parseFn);
}

#endif
