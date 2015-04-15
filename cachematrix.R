## This is the code for cached matrix inversion computation.
## The code follows the same logic as the cached vector computation.
#  1. makeCacheMatrix() makes a cached version of the original matrix.
#  2. cachesolve() first checks if there is already an inversion.
#     If not, inverse it. 
#     If so, extract the cached result.

# makeCacheMatrix creates a special "matrix", which contains 4 component functions to
# set or get the value of the matrix, or to set or get the inversion result.
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve is the function that computes matrix inversion.
## The program first checks if there is already an inversion. If not, inverse it. 
## If so, extract the cached result.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
