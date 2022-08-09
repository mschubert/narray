#include <unordered_map>
#include <string>
#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

template<int RTYPE> Vector<RTYPE> cpp_stack_impl(List array_list, int along, Vector<RTYPE> fill) {
    auto array_names = as<vector<string>>(array_list.attr("names"));
    auto dimnames = vector<vector<string>>(along); // dim: names along
    auto axmap = vector<unordered_map<string, int>>(along); // dim: element name->index
    auto a2r = vector<vector<vector<int>>>(array_list.size()); // array > dim > element

    // create lookup tables for all present dimension names
    for (int ai=0; ai<Rf_xlength(array_list); ai++) { // array index
        auto a = as<Vector<RTYPE>>(array_list[ai]);
        auto dn = as<List>(a.attr("dimnames"));
        auto da = as<vector<int>>(a.attr("dim"));
        if (along == da.size()+1) { // along introduces new dimension
            dn.push_back(array_names[ai]);
            da.push_back(1);
        }

        a2r[ai] = vector<vector<int>>(da.size());
        if (dimnames.size() < da.size()) {
            dimnames.resize(da.size());
            axmap.resize(da.size());
        }

        for (int d=0; d<da.size(); d++) { // dimension in array
            auto dni = as<vector<string>>(dn[d]);

            for (int e=0; e<da[d]; e++) { // element in dimension
                if (axmap[d].count(dni[e]) == 0) {
//                    cout << "array " << ai << " dim " << d << ": " << dni[e] << " -> " << axmap[d].size() << "\n";
                    axmap[d].emplace(dni[e], axmap[d].size());
                    dimnames[d].push_back(dni[e]);
                }
                a2r[ai][d].push_back(axmap[d][dni[e]]);
            }
        }
    }

/*    for (int ai=0; ai<a2r.size(); ai++)
        for (int di=0; di<a2r[ai].size(); di++) {
            cout << "*** array " << ai << " dim " << di << ": ";
            copy(a2r[ai][di].begin(), a2r[ai][di].end(), ostream_iterator<int>(cout, " "));
            cout << "\n";
        }
*/
    // create result array with attributes
    auto rdim = IntegerVector(dimnames.size());
    for (int ai=0; ai<Rf_xlength(array_list); ai++)
        if (a2r[ai].size() != rdim.size())
            stop("Names are required for all dimensions except the one stacked along.\n%s",
                 "  Use bind() if you want to just bind together arrays without names.");
    auto rdnames = List(dimnames.size());
    for (int i=0; i<dimnames.size(); i++) {
        rdim[i] = dimnames[i].size();
        rdnames[i] = CharacterVector(dimnames[i].begin(), dimnames[i].end());
    }
    auto n = accumulate(rdim.begin(), rdim.end(), 1, multiplies<int>());
    auto result = Vector<RTYPE>(n, fill[0]);
    result.attr("dim") = rdim;
    result.attr("dimnames") = rdnames;

    // fill the result array
    int maxdim = rdim.size() - 1;
    for (int ai=0; ai<Rf_xlength(array_list); ai++) { // each original array
        auto a = as<Vector<RTYPE>>(array_list[ai]);
        auto it = vector<vector<int>::iterator>(a2r[ai].size()); // each result dim
        for (int d=0; d<it.size(); d++)
            it[d] = a2r[ai][d].begin();

        int dim_offset;
        bool new_offset = true;
        for (int aidx=0; aidx<a.size(); aidx++) { // element in original array
            if (new_offset) { // dimension jump
                dim_offset = 0;
                int dim_mult = 1;
                for (int d=0; d<maxdim; d++) {
                    dim_mult *= rdim[d];
                    dim_offset += dim_mult * *it[d+1];
                }
                new_offset = false;
            }

//            cout << "result[" << *it[0] + dim_offset << "] = a[" << ai << "][" << aidx << "]\n";
            result[*it[0] + dim_offset] = a[aidx];

            it[0]++;
            for (int d=0; d<maxdim; d++) { // check if we're jumping dimensions
                if (it[d] != a2r[ai][d].end()) // dim+1 jump only if dim jump
                    break;

                new_offset = true;
                it[d] = a2r[ai][d].begin();
                it[d+1]++;
            }
        };
    }

    return result;
}

// [[Rcpp::export]]
SEXP cpp_stack(List array_list, int along, SEXP fill) {
    auto max_type = NILSXP;
    for (int ai=0; ai<array_list.size(); ai++) {
        int cur_type = TYPEOF(array_list[ai]);
        if (cur_type < LGLSXP || cur_type > STRSXP)
            stop("Invalid type: %d %s\n", cur_type, type2name(array_list[ai]));
        if (cur_type > max_type)
            max_type = cur_type;
    }

    switch(max_type) {
        case LGLSXP: return cpp_stack_impl<LGLSXP>(array_list, along, as<LogicalVector>(fill));
        case INTSXP: return cpp_stack_impl<INTSXP>(array_list, along, as<IntegerVector>(fill));
        case REALSXP: return cpp_stack_impl<REALSXP>(array_list, along, as<NumericVector>(fill));
        case CPLXSXP: return cpp_stack_impl<CPLXSXP>(array_list, along, as<ComplexVector>(fill));
        case STRSXP: return cpp_stack_impl<STRSXP>(array_list, along, as<CharacterVector>(fill));
        default: return R_NilValue; // this should not happen
    }
}
