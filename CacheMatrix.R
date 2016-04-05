

makeCacheMatrix <- function(x = matrix()) {
        ## Variable to hold cached inverse
        inverse <- NULL
        ## Set the underlying matrix value
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        ## Get the underlying matrix value
        get <- function() x
        ## Set the cached inverse value 
        setinv <- function(inv) inverse <<- inv
        ## Get the cached inverse value
        getinv <- function() inverse
        # Return the vector of functions for manipulating our special matrix
        list(set = set, get = get,
             setinv = setinv, getinv = getinv)
}

## Takes a special matrix created by makeCacheMatrix() and returns 
## the inverse from cache if calculated, otherwise it finds the 
## inverse and stores in cache before returning it
cacheSolve <- function(x) {
        ## Check the cache for the inverse
        inv <- x$getinv()
        ## If there, return it to caller
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## Otherwise, retrieve the raw matrix and find the inverse
        data <- x$get()
        inv <- solve(data)
        ## Store in the cache
        x$setinv(inv)
        ## Return inverse to caller
        inv  
}
