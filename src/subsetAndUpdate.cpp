#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List subsetAndUpdate(DateVector From, DateVector To, int maxDiff) {

  LogicalVector idx(From.size(), TRUE);

  int marker = 0;

  for (int i = 1; i < From.size(); i++) {

    idx[i] = (From[i] - To[marker]) > maxDiff + 1;

    if(idx) marker = i;

    else if (To[i] > To[marker]) To[marker] = To[i];

  }

  return List::create(

    _["From_out"] = From[idx],
                        _["To_out"] = To[idx]

  );

}
