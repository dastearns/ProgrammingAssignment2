## A set of functions that create and invert 
## matrices and cache the results.

## Creates a cacheable matrix for input into 
## the cacheSolve() function which sets and 
## retrieves the cached values
makeCacheMatrix <- function(x = matrix()) {
  
    # check that input is a matrix
    if (!is.matrix(x)) {
      stop("Please give a matrix")
    }
    
    # set matrix
    x.inv <- NULL
    set <- function(y) {
      x <<- y
      x.inv <<- NULL
    }
    
    # get matrix
    get <- function() x
    
    # inverse the matrix 
    set.inverse <- function(solve) x.inv <<- solve
    get.inverse <- function() x.inv
    
    #return list of functions
    list(
      set = set, 
      get = get,
      set.inverse = set.inverse,
      get.inverse = get.inverse
    )
}


## Computes the inverse of the cacheable matrix returned by makeCacheMatrix()
## If the inverse matrix has been previously calculated and the matrix has not changed 
## then the function returns the cached inverse
cacheSolve <- function(x, ...) {
  ## get inverse of supplied matrix 
  x_inv <- x$get.inverse()
  
  # check if there was a cached matrix and return
  if(!is.null(x_inv)) {
    message("Getting cached inverse matrix")
    return(x_inv)
  } else {
  # no cached matrix so create an inverted matrix and return
    x_inv <- x$get()
    x_inv <- solve(x_inv)
    x$set.inverse(x_inv)
    x_inv
  }
}