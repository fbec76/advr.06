#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List greedy_knapsack_cpp(DataFrame x, double W) {
  NumericVector v = x["v"];
  NumericVector w = x["w"];
  const R_xlen_t n = v.size();

  NumericVector ratio(n);
  for (R_xlen_t i = 0; i < n; ++i) ratio[i] = v[i] / w[i];

  Environment base = Environment::namespace_env("base");
  Function order = base["order"];
  IntegerVector ord = order(ratio, _["decreasing"] = true, _["method"] = "radix");


  double s1_w = 0.0, s1_v = 0.0;
  std::vector<int> s1; s1.reserve(64);
  for (R_xlen_t k = 0; k < n; ++k) {
    int i = ord[k] - 1;          // 1-based -> 0-based
    double wi = w[i];
    if (s1_w + wi <= W) {
      s1_w += wi;
      s1_v += v[i];
      s1.push_back(i + 1);       // return 1-based indices
    } else {
      break;
    }
  }

  int best_single = -1; double best_v = R_NegInf;
  for (R_xlen_t i = 0; i < n; ++i) {
    if (w[i] <= W && v[i] > best_v) { best_v = v[i]; best_single = i; }
  }

  double final_value;
  IntegerVector final_elements;
  if (best_single != -1 && best_v >= s1_v) {
    final_value = best_v;
    final_elements = IntegerVector::create(best_single + 1);
  } else {
    final_value = s1_v;
    final_elements = IntegerVector(s1.begin(), s1.end());
  }

  return List::create(_["value"] = final_value,
                      _["elements"] = final_elements);
}