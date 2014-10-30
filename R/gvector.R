setClass("gvector", representation(vec="list",dim="vector"))

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

as.gvector = function(vec=list(), dim=c())
{
  if (is.numeric(vec)) {
    if (missing(dim)) {
      new.gvector(vec)
    } else {
      new.gvector(vec,dim=dim)
    }
  } else {
    if (class(vec) == "gvector") {
      vec
    } else {
      new.gvector(list(vec))
    }			
  }
}

dim.gvector = function(x) x@dim

"dim<-.gvector" = function(x,value) {
  if (prod(value) != length(x@vec)) stop("Wrong dimensions for this gvector")
  new("gvector",vec=x@vec, dim=value);
}

as.gvector(1:3)

getGenericFun = function () 
{
  frame <- sys.parent()
  envir <- parent.frame()
  call <- sys.call(frame)
  localArgs <- FALSE
  if (exists(".Generic", envir = envir, inherits = FALSE)) 
    fname <- get(".Generic", envir = envir)
  else {
    localArgs <- identical(as.character(call[[1L]]), ".local")
    if (localArgs) 
      call <- sys.call(sys.parent(2))
    fname <- as.character(call[[1L]])
  }
  fdef <- get(fname, envir = envir)
  fdef
}


Ops.gvector.apply = function(e1,e2,fun) {
  n1 = length(e1@vec)
  n2 = length(e2@vec)
  i1 = 1:n1
  i2 = 1:n2
  if (n1 > n2) {
    if (n1 %% n2 != 0) {
      stop("Length of g-vectors dont match")
    }
    i2 = rep_len(i2, n1)
    d = e1@dim
  } else {
    if (n2 %% n1 != 0) {
      stop("Length of g-vectors dont match")
    }
    i1 = rep_len(i1, n2)
    d = e2@dim
  }
  vec = lapply(1:length(i1), function(i) {fun(e1@vec[[i1[i]]],e2@vec[[i2[i]]])} )
  new.gvector(vec,d)
}

Ops.gvector = function(e1,e2) {
  fun = getGenericFun()
  Ops.gvector.apply(e1,e2,fun)
}

Ops.gvector.other = function(e1,e2) {
  fun = getGenericFun()
  e1 = as.gvector(e1)
  e2 = as.gvector(e2)
  Ops.gvector.apply(e1,e2,fun)
}

setMethod("Ops", signature("gvector","gvector"), Ops.gvector)
setMethod("Ops", signature("gvector","ANY"), Ops.gvector.other)
setMethod("Ops", signature("ANY","gvector"), Ops.gvector.other)

print.gvector = function(object) {
  tp = sapply(object@vec, class)
  dim(tp) = object@dim
  print(tp)
}

setMethod("show", "gvector", print.gvector)

V = function(...) {
  l = list(...)
  if (length(l) == 0) {
    new.gvector(c())
  } else if (length(l) == 1) {
    as.gvector(l[[1]])
  } else {
    l = lapply(l, function(e) {
      if (class(e) != "gvector") e = as.gvector(e)
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

setMethod("sum", "gvector", function(x,...) {
  if (length(x@vec) > 0) {
    ret = x@vec[[1]];	
    if (length(x@vec) > 0) {
      for (i in 2:length(x@vec)) {
        ret = ret + x@vec[[i]];	
      }
    }
    if (length(list(...)) > 0) {
      ret + sum(...);
    } else {
      ret
    }
  } else {
    if (length(list(...)) > 0) {
      sum(...);
    } else {
      0
    }
  }
})

sum(V(1:3))


setMethod("[", signature("gvector","numeric","missing"), function(x,i,j,...) {
  h = 1:length(x@vec)
  h = h[i]
  ndim = length(h);
  new.gvector(x@vec[h],ndim)
})

setMethod("[", signature("gvector","numeric","numeric"), function(x,i,j,...,drop=F) {
  h = 1:length(x@vec)
  dim(h) = x@dim
  h = h[i,j,...,drop=drop]
  ndim = dim(h);
  if (is.null(ndim)) ndim=length(h)
  new.gvector(x@vec[as.vector(h)],ndim)
})

setMethod("[[", signature("gvector","numeric"), function(x,i,...) {
  x@vec[[i]]
})


calc = function(object,x,...) {
  stop("Called generic calc")
}

setGeneric("lag")
setGeneric("calc")

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


mat.prod.gvector.apply = function(x,y) {
  if (length(x@dim) > 1) {
    i1 = prod(x@dim[-length(x@dim)])
    j1 = x@dim[length(x@dim)]
  } else {
    i1 = 1
    j1 = x@dim[1]
  }	
  if (length(y@dim) > 1) {
    i2 = prod(y@dim[-1])
    j2 = y@dim[1]
  } else {
    i2 = 1
    j2 = y@dim[1]
  }
  print(c(i1,j1,i2,j2))
  if (j1 != j2) stop("Non conforming matrices in %*%");
  w = expand.grid(i=1:i1,j=1:i2)
  w = lapply(1:nrow(w),function(i) w[i,,drop=F])
  xdim=c(x@dim[-length(x@dim)],y@dim[-1])
  if (length(xdim)<1) xdim=1
  new.gvector(
    lapply(w,function(a) {
      sum(x[a$i+((1:j1-1)*i1)] * y[1:j1 + j1*(a$j-1)])
    }),
    xdim
  )
}

mat.prod.gvector.other = function(x,y) mat.prod.gvector.apply(as.gvector(x),as.gvector(y))
setMethod("%*%",signature("gvector","gvector"), mat.prod.gvector.apply)
setMethod("%*%",signature("gvector","ANY"), mat.prod.gvector.other)
setMethod("%*%",signature("ANY","gvector"), mat.prod.gvector.other)


