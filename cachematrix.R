## This function calculates the inverse of a matrix and stores the value in a cache.
## Before calculating the inverse of any matrix, it will check the cache to see if this inverse is already known.

## makeCacheMatrix Creates a list of 4 functions to set and get the input matrix and to set and get the inverse 
## of the matrix. It also creates objects m, used to store the inverse if it is known and else set to NULL, 
## and x, containing the input matrix

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL   
            set <- function(y) {
                  x <<- y
                  m <<- NULL
            }
            get <- function() x
            setInverse <- function(inverse) m <<- inverse
            getInverse <- function() m
            list(set = set, get = get,
                 setInverse = setInverse,
                 getInverse = getInverse) 


}


## cacheSolve takes the output from makeCacheMatrix and returns the inverse. If m is not NULL, 
## it uses m, else it computes the inverse using solve and assigns the inverse matrix to m.

cacheSolve <- function(x, ...) {
      m <- x$getInverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setInverse(m)
      m

        ## Return a matrix that is the inverse of 'x'
}
