## makeCacheMatrix and cacheSolve are functions that are used to 
## create and use cached inverse operation of matrix
## Calculations of inverse is computational intensive operations
## so having cached version of inverse can save us time and CPU cycles

## This function is used to create a warpper around matrix
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  ## create a setter that captures closure
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## create a getter that returns matrix
  get <- function() x
  
  ## create a setter and getter for result of inversion calculation
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  
  ## store all created methods in a list
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## This function checks weather inverse is already cached
## and returns inverse if it was previously cached otherwise 
## it calculates inverse cache it and returns result
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  
  ## check if inverse is calculated and return if it is
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## get original matrix 'x'
  data <- x$get()
  ## calculate inverse of original matrix 'x'
  m <- solve(data, ...)
  ## cache the value of inverse
  x$setsolve(m)
  
  ## return inverse
  m
}
