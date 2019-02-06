## Caching inverse of matrix
## The purpose is reduce computation by caching matrix inversion
## instead of compute repeatedly
## Contains 2 functions : macheCacheMatrix and cacheSolve

## makeCacheMatrix creates a special matrix object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setmatrix <- function(mt) inv <<- mt
  getmatrix <- function() inv
  list(set=set, 
       get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cache data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setmatrix(m)
  m
}
