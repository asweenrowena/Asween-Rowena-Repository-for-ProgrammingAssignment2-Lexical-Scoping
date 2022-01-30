#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly. The overall function of the functions below enables the retrieval of the inverse of a matrix quickly by caching the calculated inverse of a matrix. If new values are set in the matrix, the cacheSolve() function calculates the inverse of this new matrix caches the inverse of this new matrix. 

##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvmatrix <- function(invmatrix) m <<- invmatrix
  getinvmatrix <- function() m
  list(set = set, get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getinvmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)  ## Return a matrix that is the inverse of 'x'
  x$setinvmatrix(m)
  m
}
