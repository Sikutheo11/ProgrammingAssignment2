## we created two function for computing inverse of matrix (makeCacheMatrix, cacheSolve)
## below makecahematrix consist (set, get, getInverse , setInverse)
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL  ##initializing inverse to be NULL
  set <- function(y){
    x <<- y
    inv <<- NULL 
  }
  get <- function() {x} ## function for getting matrix
  setInverse <- function(inverse)  {inv <<- inverse}
  getInverse <- function() {inv} ## function for computing inverse of matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
## below function id=s for getting cache data
cacheSolve <-  function(x, ...){ ## getting cache data
  inv <- x$getInverse()
  if(!is.null(inv)){ ## check if inverse is NULL
    message("getting cached data")
    return(inv) ## values of matrix inverse
  }
  mat <-x$get() ## computing inverse values
  inv <- solve(mat, ...)
  x$setInverse(inv) 
  inv
}
