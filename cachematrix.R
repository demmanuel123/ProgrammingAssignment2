## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  # Special matrix object "x" has the following functions
  # get(): To retrieve x for all operation
  # set(): To change value of x
  # setInverse : To cache calculated inverse of x
  # getInverse : to retrive cached value of inverse of x
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function()m
  
  list (
    set=set,
    get=get,
    setInverse=setInverse,
    getInverse=getInverse
  )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverse()
  if(!is.null(m)){
     print("Returning cached data")
     return(m)
  }
    
  m <-solve(x$get())
  x$setInverse(m)
  m
  
}
