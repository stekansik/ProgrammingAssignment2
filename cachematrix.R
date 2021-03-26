## These two functions cache the inverse of a matrix 


## makeCacheMatrix function makes a special matrix that is a list containing a function that
## (1) Sets the value of the matrix (set)
## (2) Gets the value of the matrix (get)
## (3) Sets the value of the inverse (setinverse)
## (4) Gets the value of the inverse (getinverse)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function retreives the inverse from the cache if it has already been computed
## Otherwise it computes the inverse of the matrix using the solve function

cacheSolve <- function(x, ...) {
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
