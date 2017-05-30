## makeCacheMatrix creates a "matrix" object that can cache its inverse
## cacheSolve computes the inverse of the object returned by the previous function. 

## this function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  seti <- function(inverse) i <<- inverse
  geti <- function() i
  list(set = set, get = get,
       seti = seti,
       geti = geti)

}


## this function checks if the inverse has been calculated for this "matrix"
## and calculates the inverse if it has not

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$geti()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$seti(i)
  i
}
