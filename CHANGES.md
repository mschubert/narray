* 0.3.2
  * adjust tests for `testthat>=2`

* 0.2.2
  * fix bug where `split` with `NA` in `subsets` caused wrong splitting; these
    are now dropped with a warning (#5)
  * fix bug where vectors are not bound along the right dimensions (#7)
  * add `rep` functions for arrays (add `rrep` and `crep` aliases for rows and
    columns, respectively)

* 0.1.1
  * Initial release on CRAN
