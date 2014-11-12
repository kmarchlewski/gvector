#' Generic function for evaluation
#' 
#' Evaluates a polynomial, function, etc.
#' @export
calc = function(object,x,...) {
  stop("Called generic calc")
}

#' @export
setGeneric("lag")

setGeneric("calc")

#' Apply lag to all elements of a gvector
#' 
#' Applied the lag functions to all elements of a gvector
#' @param x gvector
setMethod("lag",signature("gvector"), function(x, dx, drop=T,...) {
  dx = as.matrix(dx)
  w = expand.grid(i=1:length(x@vec),j=1:nrow(dx))
  w = lapply(1:nrow(w),function(i) w[i,,drop=F])
  xdim=x@dim
  if ((xdim[1] == 1)&&(drop)) xdim=xdim[-1]
  new.gvector(
    lapply(w,function(a) {
      lag( x@vec[[a$i]], dx[a$j])
    }),
    c(xdim, nrow(dx))
  )
})

calc.gvector.apply = function(object, x) {
  x = as.matrix(x)
  w = 1:length(object@vec)
  ret = lapply(w,function(i) {
    calc( object@vec[[i]], x)
  })
  ret = do.call(c,ret)
  dim(ret) = c(nrow(x), object@dim)
  ret
}

setMethod("calc",signature("gvector","numeric"), calc.gvector.apply)
setMethod("calc",signature("gvector","array"), calc.gvector.apply)
