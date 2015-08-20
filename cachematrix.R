## R-Programming (r-prog 31) COursera HW 2
## Key concept is Lexical Scoping
## Completed 8-20-15 Lindsay Thomas Mico 


## Generates a list containing 4 functions to define and retrieve a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
        
}

## Return a matrix that is the inverse of 'x'
## If the inverse is already defined it is pulled from the list defined in makeCacheMatrix
## If the inverse is not yer defined it is calculated using the solve function
cacheSolve <- function(x, ...) {
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

