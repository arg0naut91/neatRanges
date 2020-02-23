#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List updateAndSubset(Rcpp::DateVector From, Rcpp::DateVector To, int max_gap, Rcpp::Nullable<Rcpp::List> startObjects = R_NilValue, Rcpp::Nullable<Rcpp::List> endObjects = R_NilValue) {

if (startObjects.isNull() & endObjects.isNull()) {

    Rcpp::LogicalVector idx(From.size(), TRUE);

    int marker = 0;

    for (int i = 1; i < From.size(); i++) {

        idx[i] = (From[i] - To[marker]) > (max_gap + 1);

        if (idx[i] == TRUE) marker = i;

        else if (To[i] > To[marker]) {

            To[marker] = To[i];

        }
    }

    return Rcpp::List::create(From[idx], To[idx]);
}

else if (startObjects.isNotNull() & endObjects.isNotNull()) {

    Rcpp::LogicalVector idx(From.size(), TRUE);

    int marker = 0;

    Rcpp::List endOb(endObjects);

    int endSize = endOb.size();

    Rcpp::List endFinal(endSize);

    for (int i = 1; i < From.size(); i++) {

        idx[i] = (From[i] - To[marker]) > (max_gap + 1);

        if (idx[i] == TRUE) marker = i;

        else if (To[i] > To[marker]) {

            To[marker] = To[i];

            for (int y = 0; y < endSize; y++) {

                Rcpp::CharacterVector endObj = endOb[y];

                endObj[marker] = endObj[i];

            }
        }
    }

    Rcpp::List stOb(startObjects);

    int stSize = stOb.size();

    Rcpp::List stFinal(stSize);

    for (int w = 0; w < stSize; w++) {

        Rcpp::CharacterVector stObjFull = stOb[w];

        stFinal[w] = stObjFull[idx];

    }

    for (int z = 0; z < endSize; z++) {

        Rcpp::CharacterVector endObjFull = endOb[z];

        endFinal[z] = endObjFull[idx];

    }

    Rcpp::List out(2 + endSize + stSize);

    out[0] = From[idx];
    out[1] = To[idx];

    for (int yw = 2; yw < (stSize + 2); yw++) {

        out[yw] = stFinal[yw - 2];

    }

    int tmpSize = 2 + stSize;

    for (int zw = tmpSize; zw < (endSize + tmpSize); zw++) {

        out[zw] = endFinal[zw - tmpSize];

    }

    for (int yw = 2; yw < (stSize + 2); yw++) {

        out[yw] = stFinal[yw - 2];

    }

    return out;

}

else if (startObjects.isNotNull() & endObjects.isNull()) {

    Rcpp::LogicalVector idx(From.size(), TRUE);

    int marker = 0;

    Rcpp::List stOb(startObjects);

    int stSize = stOb.size();

    Rcpp::List stFinal(stSize);

    for (int i = 1; i < From.size(); i++) {

        idx[i] = (From[i] - To[marker]) > (max_gap + 1);

        if (idx[i] == TRUE) marker = i;

        else if (To[i] > To[marker]) {

            To[marker] = To[i];

        }
    }

    for (int w = 0; w < stSize; w++) {

        Rcpp::CharacterVector stObjFull = stOb[w];

        stFinal[w] = stObjFull[idx];

    }

    Rcpp::List out(2 + stSize);

    out[0] = From[idx];
    out[1] = To[idx];

    for (int yw = 2; yw < (stSize + 2); yw++) {

        out[yw] = stFinal[yw - 2];

    }

    return out;

}

else {

    Rcpp::LogicalVector idx(From.size(), TRUE);

    int marker = 0;

    Rcpp::List endOb(endObjects);

    int endSize = endOb.size();

    Rcpp::List endFinal(endSize);

    for (int i = 1; i < From.size(); i++) {

        idx[i] = (From[i] - To[marker]) > (max_gap + 1);

        if (idx[i] == TRUE) marker = i;

        else if (To[i] > To[marker]) {

            To[marker] = To[i];

            for (int y = 0; y < endSize; y++) {

                Rcpp::CharacterVector endObj = endOb[y];

                endObj[marker] = endObj[i];

            }
        }
    }

    for (int z = 0; z < endSize; z++) {

        Rcpp::CharacterVector endObjFull = endOb[z];

        endFinal[z] = endObjFull[idx];

    }

    Rcpp::List out(2 + endSize);

    out[0] = From[idx];
    out[1] = To[idx];

    for (int yw = 2; yw < (endSize + 2); yw++) {

        out[yw] = endFinal[yw - 2];

    }

    return out;
}

}