# git head

# narray 0.5.0

  * `stack` is now compiled using `Rcpp` (#26)
  * `mask` will now make `NA` values `FALSE`
  * `melt` now keeps object names for multiple arrays (#25)

# narray 0.4.1

  * speed improvements for `construct`, `melt` (#23)
  * `lambda` adds `tbl_df` class if `simplify=FALSE` (#19)
  * `map`, `lambda`, `stack` now have progress bars after 5s (#16)
  * Package no longer depends on `reshape2`

# narray 0.4.0

  * Package no longer depends on `abind`, `pryr`
  * `along=-1` uses last dimension in `flatten`, `subset` (#6)
  * `stack` now also accepts multiple arrays as arguments
  * `summarize` is kept as `translate`
  * `construct` no longer allows value aggregation; do this on the `data.frame`
    before (related to #11)
  * `construct` now has `name_axes` option to keep column names (#12)
  * `map` subsets that are `NA` are dropped with a warning (#13)
  * `map` subsets throw error if not same length as array axis
  * new `lambda` syntax (#14)

# narray 0.3.2
  * add `collect` function as opposite to `mask`
  * deprecate `summarize` in favor of `map`
  * adjust tests for `testthat>=2`

# narray 0.2.2
  * fix bug where `split` with `NA` in `subsets` caused wrong splitting; these
    are now dropped with a warning (#5)
  * fix bug where vectors are not bound along the right dimensions (#7)
  * add `rep` functions for arrays (and `rrep` and `crep` aliases for rows and
    columns, respectively)

# narray 0.1.1
  * Initial release on CRAN
