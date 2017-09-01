## Caching the Invers of a Matrix:
## Pair of functions which create a special object that stores a matrix
## and caches its inverse

## makeCacheMatrix creates a special "matrix" object that can cache it inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function () x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list (set = set,
        get = get,
        setInv = setInv,
        getInv = getInv)

}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <-x$getInv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInv(inv)
  inv
}
