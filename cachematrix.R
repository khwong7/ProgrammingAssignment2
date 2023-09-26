## makeCacheMatrix is a function that will return a list of functions
## that can set, retrieve the input matrix, set and retrieve the inverse

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


## cacheSolve is a function that will first check for a cached matrix,
## and will otherwise solve and cache the matrix inverse for future retrieval
## and return the matrix inverse in its output

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
