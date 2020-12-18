#include "bits-and-bobs.hh"

using namespace std;

enum class ExprType { ConstantExpr, SumExpr, MultExpr, ParenExpr };

struct Expr;

typedef union {
  Expr *expr;
  long value;
} Argument;

struct Expr {
  Expr(long value) : type{ExprType::ConstantExpr}, arg1{}, arg2{} {
    arg1.value = value;
    arg2.expr = nullptr;
  }

  Expr(Expr *e1) : type{ExprType::ParenExpr}, arg1{}, arg2{} {
    arg1.expr = e1;
    arg2.expr = nullptr;
  }

  Expr(ExprType t, Expr *e1, Expr *e2) : type{t}, arg1{}, arg2{} {
    arg1.expr = e1;
    arg2.expr = e2;
  }

  ExprType type;
  Argument arg1;
  Argument arg2;
};

class ExprTree {
public:
  ExprTree(bool hasPrecedence_)
      : hasPrecedence{hasPrecedence_}, rootExpr{nullptr} {};

  ~ExprTree() {
    if (rootExpr != nullptr) {
      deque<Expr *> deleteStack;
      deleteStack.push_back(rootExpr);
      while (!deleteStack.empty()) {
        Expr *cur = deleteStack.back();
        deleteStack.pop_back();
        switch (cur->type) {
        case ExprType::ConstantExpr:
          break;
        case ExprType::SumExpr:
        case ExprType::MultExpr:
          deleteStack.push_back(cur->arg1.expr);
          deleteStack.push_back(cur->arg2.expr);
          break;
        case ExprType::ParenExpr:
          deleteStack.push_back(cur->arg1.expr);
          break;
        }
        delete cur;
      }
    }
  }

  ExprTree(const ExprTree &) = delete;
  ExprTree &operator=(const ExprTree &) = delete;
  ExprTree(ExprTree &&t)
      : hasPrecedence{t.hasPrecedence}, rootExpr{t.rootExpr} {
    t.rootExpr = nullptr;
  }
  ExprTree &operator=(ExprTree &&t) {
    hasPrecedence = t.hasPrecedence;
    std::swap(rootExpr, t.rootExpr);
    return *this;
  };

  inline bool empty() const { return rootExpr == nullptr; }

  void setValue(long value) {
    assert(empty());
    rootExpr = new Expr{value};
  }

  void setValue(ExprTree &&t) {
    assert(empty());
    rootExpr = new Expr{t.rootExpr};
    t.rootExpr = nullptr;
  }

  void addSum(long value) {
    assert(!empty());
    auto subExpr = new Expr{value};
    if (hasPrecedence) {
      if (rootExpr->type == ExprType::MultExpr) {
        auto arg2 = rootExpr->arg2.expr;
        rootExpr->arg2.expr = new Expr{ExprType::SumExpr, arg2, subExpr};
      } else {
        rootExpr = new Expr{ExprType::SumExpr, rootExpr, subExpr};
      }
    } else {
      rootExpr = new Expr{ExprType::SumExpr, rootExpr, subExpr};
    }
  }

  void addSum(ExprTree &&t) {
    assert(!empty());
    auto subExpr = new Expr{t.rootExpr};
    t.rootExpr = nullptr;

    if (hasPrecedence) {
      if (rootExpr->type == ExprType::MultExpr) {
        auto arg2 = rootExpr->arg2.expr;
        rootExpr->arg2.expr = new Expr{ExprType::SumExpr, arg2, subExpr};
      } else {
        rootExpr = new Expr{ExprType::SumExpr, rootExpr, subExpr};
      }
    } else {
      rootExpr = new Expr{ExprType::SumExpr, rootExpr, subExpr};
    }
  }

  void addMult(long value) {
    assert(!empty());
    auto subExpr = new Expr{value};
    rootExpr = new Expr{ExprType::MultExpr, rootExpr, subExpr};
  }

  void addMult(ExprTree &&t) {
    assert(!empty());
    rootExpr = new Expr{ExprType::MultExpr, rootExpr, new Expr{t.rootExpr}};
    t.rootExpr = nullptr;
  }

  void print() const {
    if (!empty()) {
      printFromNode(rootExpr);
    }
    std::cout << "\n";
  }

  long eval() const {
    assert(!empty());
    return eval(rootExpr);
  }

  /*
    I tried to come up with a way to evaluate the
    expression using a stack, instead of recursive calls,
    but I didn't succeed.

    There will be a way to evaluate these expressions using a stack
    instead of recursive calls.
  */
  long eval(const Expr *e) const {
    if (e->type == ExprType::ConstantExpr) {
      return e->arg1.value;
    } else if (e->type == ExprType::SumExpr) {
      return eval(e->arg1.expr) + eval(e->arg2.expr);
    } else if (e->type == ExprType::MultExpr) {
      return eval(e->arg1.expr) * eval(e->arg2.expr);
    } else if (e->type == ExprType::ParenExpr) {
      return eval(e->arg1.expr);
    } else {
      throw std::runtime_error("unknown expr type");
    }
  }

  static string::const_iterator
  extractParenExpr(const string &s, string::const_iterator it, string &out) {
    int openCount = 0;
    auto initIt = it;

    while (it != s.cend()) {
      if ((openCount == 0) && (*it == ')')) {
        break;
      } else if (*it == ')') {
        openCount--;
      } else if (*it == '(') {
        openCount++;
      }
      assert(openCount >= 0);
      it++;
    }
    out = s.substr(initIt - s.cbegin(), it - initIt);
    return it;
  }

  void parseFromString(const string &s) {
    assert(empty());

    auto it = s.cbegin();
    string number{""};
    char curOp = ' ';
    ExprTree subTree{hasPrecedence};
    while (it != s.cend()) {
      char op = ' ';
      if (std::isdigit(*it)) {
        number += *it;
      } else if (*it == '+') {
        op = '+';
      } else if (*it == '*') {
        op = '*';
      } else if (*it == '(') {
        string subExpr;
        it = extractParenExpr(s, it + 1, subExpr);
        subTree.parseFromString(subExpr);
      }
      if (op != ' ') {
        addOp(curOp, number, std::move(subTree));
        curOp = op;
      }
      it++;
    }
    addOp(curOp, number, std::move(subTree));
  }

private:
  void printFromNode(const Expr *cur) const {
    switch (cur->type) {
    case ExprType::ConstantExpr:
      std::cout << cur->arg1.value;
      break;
    case ExprType::SumExpr:
      printFromNode(cur->arg1.expr);
      std::cout << " + ";
      printFromNode(cur->arg2.expr);
      break;
    case ExprType::MultExpr:
      printFromNode(cur->arg1.expr);
      std::cout << " * ";
      printFromNode(cur->arg2.expr);
      break;
    case ExprType::ParenExpr:
      std::cout << "(";
      printFromNode(cur->arg1.expr);
      std::cout << ")";
      break;
    default:
      throw std::runtime_error("unknown expr type");
    }
  }

  void addOp(char op, string &number, ExprTree &&subTree) {
    if (op == ' ') {
      if (number.empty()) {
        setValue(std::move(subTree));
      } else {
        setValue(std::stol(number));
      }
    } else {
      if (op == '+') {
        if (number.empty()) {
          assert(!subTree.empty());
          addSum(std::move(subTree));
        } else {
          addSum(std::stol(number));
        }
      } else if (op == '*') {
        if (number.empty()) {
          assert(!subTree.empty());
          addMult(std::move(subTree));
        } else {
          addMult(std::stol(number));
        }
      } else {
        throw std::runtime_error("unknoen cur op");
      }
    }
    number.clear();
  }

  bool hasPrecedence;
  Expr *rootExpr;
};

void part1(const vector<string> &lines) {
  long sum = 0;
  for (const auto &line : lines) {
    ExprTree tree{false};
    tree.parseFromString(line);
    sum += tree.eval();
  }
  print(sum);
}

void part2(const vector<string> &lines) {
  long sum = 0;
  for (const auto &line : lines) {
    ExprTree tree{true};
    tree.parseFromString(line);
    sum += tree.eval();
  }
  print(sum);
}

int main() {
  auto lines = readLinesFromFile("input/day18.txt");
  part1(lines);
  part2(lines);
}
