## The following functions inverse a matrix and cache the result.
## Usage: Execute the makeCacheMatrix function first with a square/invertible matrix,
## use the returned list to call the cacheSolve function.

## This function takes a matrix (x) and returns a special "matrix" object (a list).
## The list contains set/get functions for caching, which are called by cacheSolve.
## Note: This function isn't calculating the inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        ## the "superassignment"-operator (<<-) is used to access the parent environment
        ## alternatively the assign function could the used for this
        x <<- y ## setting x to the new matrix
        m <<- NULL ## resetting m so that the inverse is generated again
    }
    get <- function() x
    
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Given a object from makeCacheMatrix this function checks whether the inverse
## has been calculated. If the inverse has been calculated it returns the result,
## otherwise it executes the solve function to construct and return the inverse.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    
    # check if the result has been calculated (is not null) and return it 
    if(!is.null(m)) {
        print("returning data from cache")
        return(m)
    }
    
    # the result is null and needs to be calculated
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    
    m
}
