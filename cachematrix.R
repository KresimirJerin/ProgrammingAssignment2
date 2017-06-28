## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## cacheSolve function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then cacheSolve should retrieve the inverse from the cache.


## makeCacheMatrix takes a matrix as an input and defines set, get, setinverse and
## getinvesre functions. Lastly, as an output it passes a list containg above functions.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## cacheSolve function takes a list made by above function as an input. It takes the matrix
## from above function and checks if it has an inverse already calculated. If not, than it
## calculates the inverse and caches it.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
  }
