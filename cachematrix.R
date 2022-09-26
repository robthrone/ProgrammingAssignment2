## The code below is designed to create a matrix cache and then get and set the 
## inverse of that matrix. Also it should be able to tell if the matrix is cache.

## Creating a matrix and getting the inverse of that matrix 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Solving for the inverse of a matrix or returning the cached value if unchanged

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

z <- makeCacheMatrix(matrix(rnorm(25), nrow = 5))
cacheSolve(z)
