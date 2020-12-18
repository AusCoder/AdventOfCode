#include "bits-and-bobs.hh"

using namespace std;

enum class ExprType {
  ConstantExpr,
  SumExpr,
  MultExpr,
  ParenExpr
}; // need ParenExpr

struct Expr;

typedef union {
  Expr *expr;
  int value;
} Argument;

struct Expr {
  Expr(int value) : type{ExprType::ConstantExpr}, arg1{}, arg2{} {
    arg1.value = value;
    arg2.expr = nullptr;
  }

  Expr(ExprType t, Expr *e1, Expr *e2) : type{t}, arg1{}, arg2{} {
    arg1.expr = e1;
    arg2.expr = e2;
  }

  Expr(ExprType t, Expr *e1) : type{t}, arg1{}, arg2{} {
    arg1.expr = e1;
    arg2.expr = nullptr;
  }

  ExprType type;
  Argument arg1;
  Argument arg2;
};

class ExprTree {
public:
  ExprTree(int value) : rootExpr{new Expr{value}} {}

  ~ExprTree() {
    deque<Expr *> deleteStack;
    deleteStack.push_back(rootExpr);
    while (!deleteStack.empty()) {
      Expr *cur = deleteStack.back();
      deleteStack.pop_back();
      if ((cur->type == ExprType::SumExpr) ||
          (cur->type == ExprType::MultExpr)) {
        deleteStack.push_back(cur->arg1.expr);
        deleteStack.push_back(cur->arg2.expr);
      }
      delete cur;
    }
  }

  ExprTree() = delete;
  ExprTree(const ExprTree &) = delete;
  ExprTree(ExprTree &&) = delete;

  void addSum(int value) {
    auto constExpr = new Expr{value};
    rootExpr = new Expr{ExprType::SumExpr, rootExpr, constExpr};
  }

  void addMult(int value) {
    auto constExpr = new Expr{value};
    rootExpr = new Expr{ExprType::MultExpr, rootExpr, constExpr};
  }

  // void addParen(ExprTree &&t) {
  //   // auto constExpr = new Expr{value};
  //   rootExpr = new Expr{ExprType::ParenExpr, rootExpr, constExpr};
  // }

  void print() const {
    printFromNode(rootExpr);
    std::cout << "\n";
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
    default:
      throw std::runtime_error("unknown expr type");
    }
  }

  Expr *rootExpr;
};

int main() {
  // auto t = ExprType::ConstantExpr;
  ExprTree tree{1};
  tree.addSum(7);
  tree.addSum(13);
  tree.print();
  print("success");
}
