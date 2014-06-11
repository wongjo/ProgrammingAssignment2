## These functions will create a cache numeric matrix x and cache the inverse matrix m,
## where x %*% m = I, I is the identity matrix.

## This function will create an object that stores a matrix x, caching in the global environment workspace
## the inverse of the matrix x may also be cached in the global environment workspace

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setsolve <- function(solve) m <<- solve
            getsolve <- function() m
            list(set = set, get = get,
                 setsolve = setsolve,
                 getsolve = getsolve)
}


## cacheSolve will calculate the inverse of x and store in in m. 
## The first call will provide the solution, the successive calls will retreive the previous calculation,
## which is stored in the global environment workspace.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            m <- x$getsolve()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setsolve(m)
            m
}
