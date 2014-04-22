## Together these functions allow the user to Cache the inverse of a given matrix for later use.


## This function, makeCacheMatrix creates a  "matrix", which is a list containing a function to
## 1.)set the value of the vector
## 2.)get the value of the vector
## 3.)set the value of the mean
## 4.)get the value of the mean


makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
             x <<- y
             m <<- NULL
      }
      get <- function() x
      setinv <- function(solve) m <<- solve
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)


}


## The following function calculates the inverse of the "matrix" created with the above function. 
## First checks to see if the inverse has already been calculated. If so, it gets the mean from the cache 
## and skips the computation. Otherwise, it calculates the inverse of the matrx and sets the value of 
## the mean in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
               message("getting cached data")
               return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
