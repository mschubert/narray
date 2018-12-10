#include <unordered_map>
#include <string>
#include <numeric>
#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
SEXP cpp_stack(SEXP arlist) {
    auto ar = as<List>(arlist);
    auto dimnames = vector<vector<string>>();
    auto axmap = vector<unordered_map<string, int>>();
    auto a2r = vector<vector<vector<int>>>(ar.size());

    // create lookup tables for all present dimension names
    for (int a=0; a<Rf_xlength(ar); a++) { // array in arlist
        auto va = as<NumericVector>(ar[a]);
        auto dn = as<List>(va.attr("dimnames"));
        auto da = as<vector<int>>(va.attr("dim"));

        a2r[a] = vector<vector<int>>(da.size());
        if (dimnames.size() < da.size()) {
            dimnames.resize(da.size());
            axmap.resize(da.size());
        }

        for (int d=0; d<da.size(); d++) { // dimension in array
            auto dni = as<vector<string>>(dn[d]);

            for (int e=0; e<da[d]; e++) { // element in dimension
                if (axmap[d].count(dni[e]) == 0) {
//                    cout << a << " @ " << d << ": " << dni[e] << " -> " << axmap[d].size() << "\n";
                    axmap[d].emplace(dni[e], axmap[d].size());
                    dimnames[d].push_back(dni[e]);
                }
                a2r[a][d].push_back(axmap[d][dni[e]]);
            }
        }
    }

    auto rdim = IntegerVector(dimnames.size());
    auto rdnames = List(dimnames.size());
    for (int i=0; i<dimnames.size(); i++) {
        rdim[i] = dimnames[i].size();
        rdnames[i] = CharacterVector(dimnames[i].begin(), dimnames[i].end());
    }
    auto n = accumulate(rdim.begin(), rdim.end(), 1, multiplies<int>());
    auto result = NumericVector(n);
    result.attr("dim") = rdim;
    result.attr("dimnames") = rdnames;

    // fill the result array
    for (int a=0; a<Rf_xlength(ar); a++) { // array in arlist
        auto va = as<NumericVector>(ar[a]);
        auto aidx = vector<int>(a2r[a].size()); // axes in orig array
        auto ridx = vector<int>(a2r[a].size()); // axes in result array
        for (int d=1; d<aidx.size(); d++)
            ridx[d] = a2r[a][d][0];

        for (int e=0; e<va.size(); e++) {
            ridx[0] = a2r[a][0][aidx[0]];
            int ridx_flat = inner_product(rdim.begin(), rdim.end()-1,
                    ridx.begin()+1, 0) + ridx[0];
            result[ridx_flat] = va[e];

/*            copy(aidx.begin(), aidx.end(), ostream_iterator<int>(cout, " "));
            cout << "-> ";
            copy(ridx.begin(), ridx.end(), ostream_iterator<int>(cout, " "));
            cout << "-- " << va[e] << " @ " << ridx_flat << "\n";
*/
            aidx[0]++;
            for (int d=0; d<aidx.size()-1; d++) {
                if (aidx[d] != a2r[a][d].size())
                    break;

                aidx[d] = 0;
                ridx[d] = a2r[a][d][0];
                ridx[d+1] = a2r[a][d+1][++aidx[d+1]];
            }
        }
//        cout << "\n";
    }

    return result;
}
