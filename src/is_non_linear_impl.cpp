#include <Rcpp.h>
#include <algorithm>

using namespace Rcpp;

// checks if an AST contains a variable
bool contains_var(CharacterVector var_names, SEXP x) {
  int x_type = TYPEOF(x);
  if (x_type == 1) { // name
    Rcpp::CharacterVector el(x);
    return std::find(var_names.begin(), var_names.end(), el[0]) != var_names.end();
  } else if (x_type == 6) { // call
    Rcpp::Language ast(x);
    for(int i = 1; i < ast.size(); i++) {
      SEXP sub_ast = ast[i].get();
      int sub_ast_type = TYPEOF(sub_ast);
      if (contains_var(var_names, sub_ast)) {
        return true;
      }
    }
  }
  return false;
}

// checks if an AST is non-linear
// [[Rcpp::export]]
bool is_non_linear_impl(CharacterVector var_names, SEXP x) {
  int x_type = TYPEOF(x);
  if (x_type == 6) { // call
    Rcpp::Language ast(x);
    if (ast.size() == 3) {
      Rcpp::CharacterVector op(ast[0]);
      std::string op_string = Rcpp::as<std::string>(op);
      if (op_string == "*" || op_string == "/" || op_string == "^") {
        bool non_linear = contains_var(var_names, ast[1]) &&
          contains_var(var_names, ast[2]);
        if (non_linear) {
          return true;
        }
      }
    }
    for(int i = 1; i < ast.size(); i++) {
      SEXP sub_ast = ast[i].get();
      int sub_ast_type = TYPEOF(sub_ast);
      if (is_non_linear_impl(var_names, sub_ast)) {
        return true;
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
