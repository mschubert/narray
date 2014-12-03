Array and Matrix processing
===========================

This modules provides some general utility functions for array programming.

The most important functions are:

 * `stack()` - like `cbind`/`rbind`, but along arbitrary axes, and taking
 care of (1) names along each dimension and (2) padding partial matching arrays
 * `map()` - like `apply`, but not modifying array dimensions and allowing
 to specify subsets that the function should be applied on; also keeps names
 * `split()` - splits an array along a given axis; can do each element or
 defined subsets
 * `intersect()` - takes a number of arrays, intersects their names along a
 given dimension, and returns sub-arrays that match in their names;
 `intersect_list` takes a list of arrays and returns a list of subsets
 * `mask()` - takes either a factor or a list of vectors and creates a binary
 matrix specifying whether each element is present
 * `construct()` - takes a data frame and a formula specifying dependent
 (values) and independent (axes) of the resulting array

### Required libraries

 * abind
 * dplyr
 * reshape2
