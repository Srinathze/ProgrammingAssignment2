## The functions below cache the inverse of  a matrix to avoid frequent computations.

## This funciton creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

	q <- NULL
	set <- function(y) {
    		x <<- y
    		q <<- NULL
  	}
  	get <- function() x
  	setsolve <- function(solve) q <<- solve
  	getsolve <- function() q
  	list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## This function get the inverse of the matrix from the cache and skips the computation.

cacheSolve <- function(x, ...) {

  q <- x$getsolve()
  if(!is.null(q)) {
    message("getting cached data")
    return(q)
  }
  data <- x$get()
  q <- solve(data, ...)
  x$setsolve(q)
  q
}
