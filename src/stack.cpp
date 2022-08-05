#include <unordered_map>
#include <string>
#include <numeric>
#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
SEXP cpp_stack(SEXP arlist) {
    auto array_list = as<List>(arlist);
    auto dimnames = vector<vector<string>>(); // dim: names along
    auto axmap = vector<unordered_map<string, int>>(); // dim: element name->index
    auto a2r = vector<vector<vector<int>>>(array_list.size()); // array > dim > element

    // create lookup tables for all present dimension names
    for (int a=0; a<Rf_xlength(array_list); a++) { // array in arlist
        auto va = as<NumericVector>(array_list[a]);
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
                    cout << "array " << a << " dim " << d << ": " << dni[e] << " -> " << axmap[d].size() << "\n";
                    axmap[d].emplace(dni[e], axmap[d].size());
                    dimnames[d].push_back(dni[e]);
                }
                a2r[a][d].push_back(axmap[d][dni[e]]);
            }
        }
    }

    for (auto ai=0; ai<a2r.size(); ai++)
        for (auto di=0; di<a2r[ai].size(); di++) {
            cout << "*** array " << ai << " dim " << di << ": ";
            copy(a2r[ai][di].begin(), a2r[ai][di].end(), ostream_iterator<int>(cout, " "));
            cout << "\n";
        }

    // create result array with attributes
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
    int maxdim = rdim.size() - 1;
    for (int ai=0; ai<Rf_xlength(array_list); ai++) {
        auto a = as<NumericVector>(array_list[ai]);
        int aidx = 0; // consecutive elements in original array

        auto it = vector<vector<int>::iterator>(a2r[ai].size()); // one for each result dim
        for (int d=0; d<it.size(); d++)
            it[d] = a2r[ai][d].begin();

        int dim_offset = 0;
        for (int d=0; d<maxdim; d++)
            dim_offset += rdim[d] * *it[d+1];

        do {
            int cur_offset = *it[0];
            int dim_offset_new = 0;
            it[0]++;
            for (int d=0; d<maxdim; d++) {
                if (it[d] != a2r[ai][d].end()) // no dimension jump
                    break;

                dim_offset_new += rdim[d]; // add only dimensions we jump
                it[d] = a2r[ai][d].begin();
                it[d+1]++;
            }

            cout << "result[" << cur_offset + dim_offset << "] = a[" << ai << "][" << aidx << "]\n";
            result[cur_offset + dim_offset] = a[aidx++];
            dim_offset += dim_offset_new;
        } while(it[maxdim] != a2r[ai][maxdim].end());
    }

    return result;
}
