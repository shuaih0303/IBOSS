library(Rcpp)
library(inline)
#x(Rx) should be like renaming the numeric vector 
src <- '
#include <algorithm>
#include <iostream>
Rcpp::NumericVector x(Rx);
Rcpp::NumericVector f(Rf);
Rcpp::NumericVector r(Rr);
Rcpp::NumericVector max(Rmax);
int n = x.size(), nf = f.size();
int k = r[0], j=0, mx = max[0], loc[mx];
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
'
getIdx <- cxxfunction(signature(Rx="numeric", Rf="numeric",
                                Rr="numeric", Rmax="numeric"),
                      src, plugin = "Rcpp")
## when you call this function in your r code, 
## Rx should be the vector to be sorted(all former selected removed) while
## Rf should be the original frame(without removing selected by former columns),
## this function will return the id in the original framework then


## a example:
### idx.oD <- getIdx(Z[,1], Z[,1], r, k)#x f r max
### for(j in 2:d) {
###   tmp <- getIdx(Z[-idx.oD,j], Z[,j], r, k)
###   idx.oD <- c(idx.oD, tmp)
### }

