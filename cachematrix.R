## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# The first function is a set of functions with the next purposes:
# set() stores the matrix that is passed as parameter of makeCacheMatrix(x) as a global variable
# get() is a generic function to that takes one parameter and returns it (in this case, it returns the initial matrix)
# setinverse() creates a function that solves (inverts) the matrix and stores it in cache
# getinverse() is a generic function to that takes one parameter and returns it (in this case, it returns the inverted matrix)
# Finally, the function returns a list of all the previous functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# First 'm' stores the inverse of the matrix (if it exists)
# The if statement checks whether it exists or not. If it does, it rerurns the inverted matrix and exits the function
# If the inverse of the matrix isn't cached, 'data' stores the original matrix passed on makeCacheMatrix()
# Then 'm' solves the matrix stored in m
# Finally x$setinverse inverts the matrix and stores it in cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
      message("m is chached")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

# TESTING THE FUNCTIONS:
A <- matrix(c(3,5,2,5,6,2,5,5,2),nrow=3,ncol=3)
M <- makeCacheMatrix(A)
cacheSolve(M)

