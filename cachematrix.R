## Put comments here that give an overall description of what your
## functions do
# "The following functions caches the inverse of a matrix, where the first 
# function caches its inverse and the second computes the inverse of the special 'matrix'/list returned by
# makeCacheMatrix." 

## Write a short comment describing this function
# "This function creates a special 'matrix' object that can cache its inverse,
# which is really a list containing a function to do no. 1-5, below"

makeCacheMatrix <- function(x = matrix()) {
  #1. set the value of the matrix
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
  #2. get the value of the matrix
    get <- function() x
  #3. set the value of the inverse
    setinverse <- function(inverse) i <<- inverse
  #4. get the value of the inverse
    getinverse <- function() i
  #5. list
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
# "This function computes the inverse of the special 'matrix'/list returned by
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cacheSolve should retrieve the
# inverse from the cache"

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

