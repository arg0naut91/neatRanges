#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List updateAndSubset(DateVector From, DateVector To, int max_gap) {

  LogicalVector idx(From.size(), TRUE);

  int marker = 0;

  for (int i = 1; i < From.size(); i++) {

    idx[i] = (From[i] - To[marker]) > (max_gap + 1);

    if (idx[i] == TRUE) marker = i;

    else if (To[i] > To[marker]) To[marker] = To[i];

  }

  return List::create(
    _["From_out"] = From[idx],
    _["To_out"] = To[idx]
  );

}
