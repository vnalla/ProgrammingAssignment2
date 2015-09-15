## Calculation of inverse matrix is time consuming process. 
## If the invers of same matrix is used multiple 
## times it is better to cache the calculated inverse and reuse it.
## Which can be saved in a variable and reused. However it will be error prone
## and the developer has to keep track of which matrix inverse is stored in which 
## variable. Instead of storing the calcualted value in a variable the original matrix and its inverse
## is stored in a special matrix which is a list of function as defined below, such the matrix and its
## inverse are together, which can alos keep track whether the inverse calculation is required or not.

## makeCacheMatrix creates a special "matrix", which is really a list
## containing the functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse matrix
## 4. get teh value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinverse <- function(inv) {
    inverse <<- inv
  }
  
  getinverse <- function() {
    inverse
  }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function calculates the inverse of the special matrix
## created using makeCacheMatrix function defined above. It checks for whether the inverse
## is already calculated or not, and calculates only of it was not calculated, and returns the inverse
## The calculated inverse is sotred in the special matrix created using makeCacheMatrix to avoid recalculation 
## of the inverse if the same special used again.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
