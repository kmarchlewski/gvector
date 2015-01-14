#' Package for generalized vector in R
#' 
#' Generalized vector is just a list, but with the arthmetic of a vector or a matrix
#' 
#' The only function you're likely to need from \pkg{gvector} is
#' \code{\link{new.gvector}}.
#'
#' @docType package
#' @name gvector-package
#' @rdname gvector-package
NULL

#' Generalized vector for R
#' 
#' @slot vec List of elements of the vector/matrix (elements can be of different types)
#' @slot dim Vector of dimensions of the vector/matrix (prod(dim) = length(vec))
#' @exportClass gvector
gvector <- setClass("gvector", representation(vec="list",dim="vector"))

#' Creates a gvector out of a list
#' 
#' @param vec List to be converted into a gvector
#' @param dim Vector of dimensions (like dim in a matrix). You should ensure that prod(dim) = length(vec).
#' @export
new.gvector = function(vec=list(), dim=c())
{
  if (missing(dim)) {
    if (is.null(dim(vec))) {
      dim = length(vec)
    } else {
      dim = dim(vec)
    }
  }
  if (is.numeric(vec)) {
    vec = lapply(1:length(vec), function(i) vec[i])
  }
  if (prod(dim) != length(vec)) stop("Wrong dimensions in V (constructor of gvector)")
  new("gvector",vec=vec, dim=dim);
}

#' Converts an object to gvector
#' 
#' @param vec Object to be converted
#' @param dim Vector of dimensions (like dim in a matrix).
#' @export
as.gvector = function(vec=list(), dim=c())
{
  if (is.numeric(vec) | all(class(vec) == "list")) {
    if (missing(dim)) {
      new.gvector(vec)
    } else {
      new.gvector(vec,dim=dim)
    }
  } else {
    if (all(class(vec) == "gvector")) {
      vec
    } else {
      new.gvector(list(vec))
    }			
  }
}


#' Convinience function for creating a gvector
#' 
#' @param ... All the things to put in a gvector
#' @export
V = function(...) {
  l = list(...)
  if (length(l) == 0) {
    new.gvector(c())
  } else if (length(l) == 1) {
    as.gvector(l[[1]])
  } else {
    l = lapply(l, function(e) {
      if (any(class(e) != "gvector")) e = as.gvector(e)
      if (length(e@dim) != 1) stop("can contacinate only 1D vectors")
      e
    })
    vec = lapply(l, function(e) e@vec)
    vec = do.call(c, vec)
    dim = sapply(l, function(e) e@dim)
    dim = sum(dim)
    new.gvector(vec,dim)
  }
}

#' @export
c.gvector = function(...,recursive=FALSE) {
  l = list(...)
  
    l = lapply(l, function(e) {
      if (any(class(e) != "gvector")) e = as.gvector(e)
      if (length(e@dim) != 1) stop("can contacinate only 1D vectors")
      e
    })
    vec = lapply(l, function(e) e@vec)
    vec = do.call(c, vec)
    dim = sapply(l, function(e) e@dim)
    dim = sum(dim)
    new.gvector(vec,dim) 
}

#' @export
length.gvector = function(x) length(x@vec)


