## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  print(x)
 
  m <- NULL
  #print("m <- x")
  setInverse <- function(solve)m <<- solve
  getInverse <- function()m
  
  list (
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
    
  print(x)
  m <-solve(x)
  x$setInverse(m)
  print(m)
  m
  
}
