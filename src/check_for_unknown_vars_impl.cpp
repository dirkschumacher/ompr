#include <Rcpp.h>
#include <algorithm>
#include <stack>

using namespace Rcpp;

// Checks for unknown variables in expression
// [[Rcpp::export]]
void check_for_unknown_vars_impl(List model, SEXP x) {
  std::stack<SEXP> stack;
  stack.push(x);
  List model_vars = model["variables"];
  std::string error_msg =
      "The expression contains a variable that is not part of the model.";
  while (!stack.empty()) {
    SEXP local_obj = stack.top();
    stack.pop();
    int local_obj_type = TYPEOF(local_obj);
    bool is_call = local_obj_type == LANGSXP;
    if (local_obj_type == SYMSXP) {
      try {
        CharacterVector var = local_obj;
        std::string var_name = as<std::string>(var[0]);
        List model_var = model_vars[var_name];
        int arity = model_var["arity"];
        if (arity > 0) {
          throw std::invalid_argument("");
        }
      } catch (...) {
        stop(error_msg);
      }
    }
    if (is_call) {
      Language ast(local_obj);
      int n_size = ast.size();
      if (n_size >= 3) {
        CharacterVector op(ast[0]);
        std::string op_string = as<std::string>(op);
        if (op_string == "[" && TYPEOF(ast[1]) == SYMSXP) {
          CharacterVector var = ast[1];
          std::string var_name = as<std::string>(var[0]);
          try {
            List var = model_vars[var_name];
            CharacterVector instances = var["instances"];

            // build search key
            std::ostringstream str;
            for (int i = 2; i < n_size; i++) {
              CharacterVector idx = ast[i];
              str << as<std::string>(idx);
              if (i != n_size - 1) {
                str << "_";
              }
            }
            if (n_size > 1) {
              CharacterVector search_key = str.str();
              bool found = std::find(instances.begin(), instances.end(),
                                     search_key[0]) != instances.end();
              if (!found) {
                throw std::invalid_argument("");
              }
            }
          } catch (...) {
            stop(error_msg);
          }
          continue;
        }
      }
      for (int i = 1; i < ast.size(); i++) {
        stack.push(ast[i].get());
      }
    }
  }
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
m <- add_variable(add_variable(MIPModel(), x[i], i = 1:10), z[i, j], i = 1, j =
1)
check_for_unknown_vars_impl(add_variable(MIPModel(), y), substitute(y))
check_for_unknown_vars_impl(m, substitute(z))
check_for_unknown_vars_impl(m, substitute(x[1]))
check_for_unknown_vars_impl(m, substitute(x[11]))
check_for_unknown_vars_impl(m, substitute(z[1, 1]))
check_for_unknown_vars_impl(m, substitute(x[1] + y[1]))

*/
