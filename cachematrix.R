## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function takes a matrix and returns an object which has the ability to cache the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(invers) inv <<- invers
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
# This function takes an object which had been created in makeCacheMatrix(),
# It then gets the cached inverse and returns it. If the inverse had not yet been cached,
# then this function generates the inverse (Solves the matrix), caches the solution, and returns the inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
