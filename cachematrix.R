## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a vector thats really a list with functions to set the matrix x, get the matrix,
## set the inverse, and get the inverse of the matrix x

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setinversemat <- function(inversemat) invmat <<- inversemat
  getinversemat <- function() invmat
  list(set = set, get = get,
       setinversemat = setinversemat,
       getinversemat = getinversemat)
}

## Write a short comment describing this function
## This function calculates inverse of cached matrix created with above function. It checks to 
## see if inverse has already been calculated and returns it. Else, it calculates the inverse 
## of the matrix x, sets the inverse in the cache using setinversemat function, and returns inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invm <- x$getinversemat()
  if(!is.null(invm)) {
    message("getting cached inverse matrix")
    return(invm)
  }
  matr <- x$get()
  invm <- solve(matr, ...)
  x$setinversemat(invm)
  invm
}
