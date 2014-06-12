## These 2 functions will create a numeric matrix x and the calculated inverse
## matrix m, ## where x %*% m = I, I is the identity matrix, cached in the 
## global environment workspace of R.


## makeCacheMatrix function will create an object that stores a matrix x, 
## caching in the global environment. The inverse of the matrix x, m, once 
## calculated, will also be cached in the global environment workspace

makeCacheMatrix <- function(x = matrix()) {
       ## initialize m within the function
       m <- NULL

	## set will cache the matrix x, and initialize m
       set <- function(y) {
               x <<- y
               m <<- NULL
       }

	## get will retrieve the matrix x
       get <- function() x

	## setsolve will cache the inverse matrix m
       setsolve <- function(solve) m <<- solve

	## getsolve will retrieve the inverse matrix m
       getsolve <- function() m

	## the list object returned
       list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve will calculate the inverse of x and store it in m. 
## The first call will provide the solution, the successive calls will retrieve 
## the previous calculation, which is stored in the global environment
## workspace. Single quotes used in comments since these are the proxy 
## variable calls for their equivalents in the previous function.
## e.g. 'x' in cacheSolve is calling the x in makeCacheMatrix

cacheSolve <- function(x, ...) {

       ## Return a matrix 'm' that is the inverse of 'x'
       m <- x$getsolve()

	## check if 'm' has already been calculated, if so, then retrieve 'm'
       if(!is.null(m)) {
               message("getting cached data")
               return(m)
       }

	## retrieve the matrix 'x'
       data <- x$get()

	## calculate inverse of matrix 'x'
       m <- solve(data, ...)

	## cache 'm'
       x$setsolve(m)

	## return 'm'
       m
}
