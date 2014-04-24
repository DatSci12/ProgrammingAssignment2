## Matrix inversion is usually a costly computation. Caching the inverse of a matrix makes execution more efficient
## rather than calculating the inverse repeatedly. The functions below allow to cache the inverse of a matrix.

## Assumption: the given matrix is a "square" matrix and is always invertible
# 
## makeCacheMatrix function creates a special "vector", which is really a list containing a function to
## a) set the value of the matrix
## b) get the value of the matrix
## c) set the value of the inverse of that matrix
## d) get the value of the inverse of that matrix  

makeCacheMatrix <- function(x = matrix()) {
  cachInv <- NULL
  set <- function(y) {
    x <<- y
    cachInv <- NULL
  }
  get <- function() x
  setInv <- function(solve) cachInv <<- solve
  getInv <- function() cachInv
  list(set = set, get = get, 
       setInv = setInv
       getInv = getInv)
}


## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix function
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.
## 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrixInv <- x$getinverse()
  if(!is.null(matrixInv)) {
    message("getting cached data")  #display console message that data is being retrieved from cache
    return(matrixInv)
  }
  data <- x$get()
  matrixInv <- solve(data, ...)
  x$setInv(matrixInv)
  matrixInv
}
