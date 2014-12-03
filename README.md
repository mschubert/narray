Array and Matrix processing
===========================

This modules provides some general utility functions for array programming.
See the `roxygen2` annotation for a more detailed description using `?function`.

A summary of the most important functions is listed below.

#### `stack()` - stacking multiple arrays together

Like `cbind`/`rbind`, but along arbitrary axes, and taking care of (1) names 
along each dimension and (2) padding partial matching arrays.

```r
```

#### `map()` - applying a function, also with subsets

Like `apply`, but not modifying array dimensions and allowing to specify 
subsets that the function should be applied on; also keeps names.

```r
```

#### `split()` - splitting an array, also with subsets

Splits an array along a given axis; can do each element or defined subsets.

```r
```

#### `intersect()` - subsetting names along a given dimension

Takes a number of arrays, intersects their names along a given dimension,
and returns sub-arrays that match in their names; `intersect_list` takes 
a list of arrays and returns a list of subsets.

```r
```

#### `mask()` - creating a binary matrix

Takes either a factor or a list of vectors and creates a binary matrix 
specifying whether each element is present.

```r
```

#### `construct()` - creating an array from a data.frame

Takes a data frame and a formula specifying dependent (values) and independent
(axes) of the resulting array.

```r
```

### Required libraries

 * abind
 * dplyr
 * reshape2
