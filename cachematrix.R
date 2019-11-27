## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Below are two functions that are used to create a special object that stores a matrix and caches its inverse.
## For this we use <<- operator which can be used to assign a value to an object in an environment that is different from the current environment
## Because getting the inverse of a matrix is an expensive function it might be beneficial to have cashing
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## Set the value of the matrix
      set <- function(y) {
        x <<- y
        m <<- NULL
      }
      ## Get the value of the matrix
      get <- function() x
      ## Respectively set and get the value of the mean
      setinv <- function(inv) m <<- inv
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## The following function calculates the inverse of the special "matrix" created with the above function.
        ##However, it first checks to see if the inverse has already been calculated.
        ##If so, it gets the inverse from the cache and skips the computation.
        ##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinv function.
        i <- x$getinv()
          if(!is.null(i)) {
            message("getting cached data")
            return(i)
          }
          data <- x$get()
          i <- inv(data, ...)
          x$setinv(i)
          i
}
