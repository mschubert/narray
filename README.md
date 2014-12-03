Array and Matrix processing
===========================

This modules provides some general utility functions for array programming.
See the `roxygen2` annotation for a more detailed description using `?function`.

A summary of the most important functions is listed below.

#### `stack()`

Like `cbind`/`rbind`, but along arbitrary axes, and taking care of (1) names 
along each dimension and (2) padding partial matching arrays.

```r
A = matrix(1:4, nrow=2, ncol=2, dimnames=list(c('a','b'),c('x','y')))
B = matrix(5:6, nrow=2, ncol=1, dimnames=list(c('b','a'),'z'))

C = stack(list(A, B), along=2)
#    x y z
#  a 1 3 6   # B is stacked correctly according to its names
#  b 2 4 5

D = stack(list(A, C), along=3)
# , , 1          , , 2
#
#   x y  z         x y z
# a 1 3 NA       a 1 3 6
# b 2 4 NA       b 2 4 5
```

#### `map()`

Like `apply`, but not modifying array dimensions and allowing to specify 
subsets that the function should be applied on; also keeps names.

```r
map(D, along=1, function(x) sum(x, na.rm=TRUE)) #FIXME: error
```

#### `split()`

Splits an array along a given axis; can do each element or defined subsets.

```r
split(C, along=2, subsets=c('s1','s1','s2'))
# $s1          $s2
#   x y        a b   # each subset is split into a separate array
# a 1 3        5 6
# b 2 4
```

#### `intersect()`

Takes a number of arrays, intersects their names along a given dimension,
and returns sub-arrays that match in their names; `intersect_list` takes 
a list of arrays and returns a list of subsets.

```r
E = C[,c(2,3,1)]
#   y z x
# a 3 6 1
# b 4 5 2

ar$intersect(A, E, along=2)
# > A         > E
#   x y         x y   # along dimension 2, all arrays have same extent
# a 1 3       a 1 3   # and same order of names; this function modifies
# b 2 4       b 2 4   # values in-place
```

#### `mask()`

Takes either a factor or a list of vectors and creates a binary matrix 
specifying whether each element is present.

```r
```

#### `construct()`

Takes a data frame and a formula specifying dependent (values) and independent
(axes) of the resulting array.

```r
```

### Required libraries

 * abind
 * dplyr
 * reshape2
