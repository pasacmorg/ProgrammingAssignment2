## Computing the inverse of a matrix can be an expensive operation.
## These two functions provide a way to invert a matrix and cache
## the result so that subsequent calls retreive the previously 
## calculated result rather than calculating it anew
##
## Acknowledgement: These two functions used the code from the 
## example mean caching functions as a starting point
## 
## Sample invocation using hilbert function to create matrix from 
## example section of solve {base} help file:
##  
## hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
## h8 <- hilbert(8);
## hc <- makeCacheMatrix(h8)
## cacheSolve(hc)


## makeCacheMatrix: This function returns an object that stores 
## inut maxtrix x and can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve: This function takes as input the object returned by 
## the makeCacheMatrix function above and computes the inverse of 
## its matrix, caching the result so that future calls simply 
## return the previously calculated inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
