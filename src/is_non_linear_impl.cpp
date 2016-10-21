#include <Rcpp.h>
#include <algorithm>
#include <stack>

using namespace Rcpp;

// checks if an AST contains a variable
bool contains_var(CharacterVector var_names, SEXP x) {
  std::stack<SEXP> stack;
  stack.push(x);
  while (!stack.empty()) {
    SEXP local_obj = stack.top();
    stack.pop();
    int local_obj_type = TYPEOF(local_obj);
    if (local_obj_type == SYMSXP) { // name
      CharacterVector el(local_obj);
      if (std::find(var_names.begin(), var_names.end(), el[0]) !=
          var_names.end()) {
        return true;
      }
    } else if (local_obj_type == LANGSXP) { // call
      Language ast(local_obj);
      for (int i = 1; i < ast.size(); i++) {
        stack.push(ast[i].get());
      }
    }
  }
  return false;
}

// checks if an AST is non-linear
// [[Rcpp::export]]
bool is_non_linear_impl(CharacterVector var_names, SEXP x) {
  std::stack<SEXP> stack;
  stack.push(x);
  while (!stack.empty()) {
    SEXP local_obj = stack.top();
    stack.pop();
    int local_obj_type = TYPEOF(local_obj);
    if (local_obj_type == LANGSXP) { // call
      Language ast(local_obj);
      if (ast.size() == 3) {
        CharacterVector op(ast[0]);
        std::string op_string = as<std::string>(op);
        if (op_string == "*" || op_string == "/" || op_string == "^") {
          bool non_linear = contains_var(var_names, ast[1]) &&
                            contains_var(var_names, ast[2]);
          if (non_linear) {
            return true;
          }
        }
      }
      for (int i = 1; i < ast.size(); i++) {
        stack.push(ast[i].get());
      }
    }
  }
  return false;
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
is_non_linear_impl(c("x", "y"), substitute(-1 * y * x))
*/
