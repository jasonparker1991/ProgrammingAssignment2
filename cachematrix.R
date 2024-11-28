## The first function, makeCacheMatrix, creates a special “matrix” object that can cache its inverse
## The second function, cacheSolve, finds the inverse of the special “matrix” returned by the makeCacheMatrix function
## If this inverse has already been computed, then the cacheSolve function retrieves the inverse from the cache

## This function creates a special “matrix” object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function finds the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
