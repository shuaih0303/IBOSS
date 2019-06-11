#include <iostream>
#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]


//' Get IBOSS subsample from one dimension 
//' 
//' @param x A data frame or data.table
//' @param f para2
//' @param rr para3
//' @param max para4
NumericVector get_iboss(NumericVector x, Rcpp::NumericVector f,
                              Rcpp::NumericVector rr, Rcpp::NumericVector max){
  int n = x.size(), nf = f.size();
  int k = rr[0], j=0, mx = max[0], loc[mx];
  double y[n];
  for (int i = 0; i < n; i++) y[i] = x[i];
  std::nth_element(y, y + k - 1, y + sizeof(y)/sizeof(*y));//then y be come partial sorted ver of x
  double  kl = y[k-1];
  for (int i = 0; i < n; i++) y[i] = -x[i];
  std::nth_element(y, y + k - 1, y + sizeof(y)/sizeof(*y));
  double  ku = -y[k-1];
  for (int i = 0; i < nf; i++) {
    if (f[i] <= kl || f[i] >= ku)
      loc[j++] = i + 1;
  }
  Rcpp::NumericVector rt(j);
  for (int l = 0; l < j; l++) {
    rt[l] = loc[l];
  }
  return rt;
}
