## This functions written for the purpose of coursera assignment
## github - atickk

## This function is for a matrix object, that can be cache its inverse

makeCacheMatrix <- function(x = matrix()) { ## define the argument with default mode of "matrix"
  inv <- NULL                               ## initialise inv as NULL to hold value of matrix inverse
  set <- function(y) {                      ## define the set function to assign new
    x <<- y                                 ## value of matrix in parent environment
    i <<- NULL                              ## if there is a new matrix, reset inv to NULL
  }
  get <- function() x                       ## define the get function and return the value of matrix argument
  setinverse <- function(inverse) i <<- inverse ## assigns value of inv in parent environment
  getinverse <- function() i                    ## gets the value of inv where called
  list(set = set,
       get = get,
       setinverse = setinverse,                 ## to refer to the functions
       getinverse = getinverse)
}


## This function computes the Inverse of special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
