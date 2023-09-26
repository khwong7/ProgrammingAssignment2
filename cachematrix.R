## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y) { # function that can be used to set a new matrix
    x <<- y
    n <<- NULL
  }
  get <- function() x # function to retrieve input matrix
  setinverse <- function(inverse) n <<- inverse # stores computed inverse
  getinverse <- function() n # function that retrieves stored inverse
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  n <- x$getinverse() # checks whether inverse has already been cached
  if (!is.null(n)){
    message("getting cached data") # retrieves cached inverse if it is not NULL
    return(n)
  }
  data <- x$get() # retrieves the matrix input
  n <- solve(data) # computes inverse
  x$setinverse(n) # stores inverse
  n # prints inverse
}
