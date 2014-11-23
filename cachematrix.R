
## Functions below are used to cache the inverse of a matrix

## makeCacheMatrix function is used to create a matrix object that can
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix


makeCacheMatrix <- function(M = matrix() ) {
  
  ## Initialize the inverse
  i <- NULL
  
  ## sets the matrix
  set <- function(matrix) {
    M <<- matrix
    i <<- NULL
  }
  
  
  
  makeCacheMatrix <- function(M = matrix()) {
    i <- NULL
    set <- function(y) {
      M <<- y
      i <<- NULL
    }
    get <- function() M
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(
      set = set,
      get = get,
      setinverse = setinverse,
      getinverse = getinverse)
  }
    
  ## gets the matrix
  get <- function() {
    ## returns the matrix
    M
  }
  
  ## sets the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## gets the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    i
  }
  
  ## Returns a list from oerations above
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the special matrix returned by 
## the makeCacheMatrix function above.
## It initially checks if the inverse has already been calculated.
## If the inverse has already been calculated (and the matrix has not
## changed), it gets the inverse from the cache and skips the computation.
## Otherwise, it compues the inverse of the matrix and sets the value of the inverse
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  
  ## returns matrix that is the inverse of 'x'
  M <- x$getInverse()
  
  ## returns the inverse if it is already calculated
  if( !is.null(M) ) {
    message("cached data")
    return(M)
  }
  
  ## gets the matrix from the object
  data <- x$get()
  
  ## computes the inverse 
  M <- solve(data, ...)
  
  ## sets the inverse to the object
  x$setInverse(M)
  
  ## return the matrix
  M
}













