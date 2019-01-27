## These two functions make it so the inverse of a matrix need not be calculated
## each time it is needed. They store the value of both the matrix and its
## inverse in a cache using lexical scoping to be used when needed. 

## makeCacheMatrix creates a list of "setter" and "getter" functions for both  
## the matrix, x, and its inverse, inv.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve checks to see if inv is a null value. If it is not, it gives a
## message and returns the value stored in inv. If it is a null, it calculates
## the inverse of the matrix and stores this value back in the original object 
## as inv.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
