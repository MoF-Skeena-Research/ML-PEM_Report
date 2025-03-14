#include <Rcpp.h>

using namespace Rcpp;
using namespace std;

struct asset_info {
  double sum, sum2, stdev;
};

//[correlation matrix](http://en.wikipedia.org/wiki/Correlation_and_dependence).
// n,sX,sY,sXY,sX2,sY2
// cor = ( n * sXY - sX * sY ) / ( sqrt(n * sX2 - sX^2) * sqrt(n * sY2 - sY^2) )
inline asset_info compute_asset_info(const NumericMatrix& mat, 
                                     const int icol, const int rstart, const int rend) {
  double sum, sum2;
  sum = sum2 = 0;
  
  for (int r = rstart; r < rend; r++) {
    double d = mat(r, icol);
    sum += d;
    sum2 += pow(d,2);
  }
  
  asset_info res;
  res.sum = sum;
  res.sum2 = sum2;
  res.stdev = sqrt((rend-rstart) * sum2 - pow(sum, 2));
  return res;
}

inline NumericMatrix c_cor_helper(const NumericMatrix& mat, const int rstart, const int rend) {
  int nc = mat.ncol();
  int nperiod = rend - rstart;
  NumericMatrix rmat(nc, nc);
  
  vector<asset_info> info(nc);
  for (int c = 0; c < nc; c++)
    info[c] = compute_asset_info(mat, c, rstart, rend);
  
  for (int c1 = 0; c1 < nc; c1++) {
    for (int c2 = 0; c2 < c1; c2++) {
      double sXY = 0;
      
      for (int r = rstart; r < rend; r++)
        sXY += mat(r, c1) * mat(r, c2);
      
      rmat(c1, c2) = (nperiod * sXY - info[c1].sum * info[c2].sum) / (info[c1].stdev * info[c2].stdev);
    }
  }
  
  return rmat;
}

// [[Rcpp::export]]
NumericMatrix c_cor(NumericMatrix mat) {
  return c_cor_helper(mat, 0, mat.nrow());
}
////////////////////////////////////////////////



IntegerVector hist(NumericVector data, NumericVector breaks){
  unsigned int bin;
  unsigned int SAMPLE_COUNT = data.length();
  unsigned int BIN_COUNT = breaks.length();
  IntegerVector out(BIN_COUNT);
  double sample;
  for (unsigned int sampleNum = 0; sampleNum < SAMPLE_COUNT; ++sampleNum){
    sample = data[sampleNum];
    bin = BIN_COUNT;
    for (unsigned int binNum = 0; binNum < BIN_COUNT; ++binNum)  {
      const int rightEdge = breaks[binNum];
      if (sample <= rightEdge) {
        bin = binNum;
        break;
      }
    }
    out[bin]++;
  }
  return(out);
}


// [[Rcpp::export]]
double obj_fn(NumericMatrix x, NumericMatrix strata, NumericMatrix cor_full, int eta = 1){
  int num_vars = x.ncol();
  int num_obs = strata.nrow();
  NumericVector hist_cnt;
  IntegerMatrix hist_out(num_obs, num_vars);
  NumericVector data;
  NumericVector strata_curr;
  NumericVector hist_temp;
  double obj_cont;
  
  for(int i = 0; i < num_vars; i++){
    data = x(_,i);
    strata_curr = strata(_,i);
    hist_out(_,i) = hist(data,strata_curr);
  }
  obj_cont = sum(abs(hist_out - eta));
  //Rcout << "Hist_out: " << temp << "\n";
  NumericMatrix cor_new = c_cor(x);
  double obj_cor = sum(abs(cor_full - cor_new));
  double out = obj_cont + obj_cor;
  
  return(out);
}


//auto h = make_histogram(axis::regular<>(6, -1.0, 2.0, "x"));
//auto h = make_histogram(axis::variable<>(strata(_,i)));
//auto data = x(_,i);
//std::for_each(data.begin(), data.end(), std::ref(h));
//h.fill(data);
//hist_cnt = h.at(Rcpp::seq(0, num_obs))
//hist_out(_,i) = hist_cnt;