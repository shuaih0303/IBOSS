#include <iostream>
#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;


//' Get IBOSS sample from one dimension 
//' 
//' @param x current remainder of one column data frame or vector
//' @param f full one column dataframe or vector
//' @param rr integer, number of selected samples, rr = k / d / 2
//' @param max integer, total subsample size, k
//' @return vector of selected indices in full dataframe.
// [[Rcpp::export]]
NumericVector getIdx(NumericVector x, NumericVector f,
                              NumericVector rr, NumericVector max){
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
  NumericVector rt(j);
  for (int l = 0; l < j; l++) {
    rt[l] = loc[l];
  }
  return rt;
}
