## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    i <- NULL
  }
  getinverse <- function() i
  setinverse <- function(inverse) i <<- inverse

  list(set = set, get = get,
       getinverse = getinverse,
       setinverse = setinverse)
}


## Ccomputes the inverse of the special "matrix" returned by `makeCacheMatrix`` above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
