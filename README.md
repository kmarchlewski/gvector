gvector - Generalized vector/matrix for R
=========

[![Build Status](https://travis-ci.org/llaniewski/gvector.svg)](https://travis-ci.org/llaniewski/gvector)

With gvector you can create a list (with elements of different types) and make calculations the same way you do with a vector or a matrix.
```r
polynomial = poly.from.roots(c(1,2,3)) # take a polynomial
value = 1.3 # take some value
A = V(polynomial, value) # make a vector out of them
# now, calculate !
B = A*A
C = diag(2) %*% A
```
