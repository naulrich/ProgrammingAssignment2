## Put comments here that give an overall description of what your
## functions do


# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        s <- NULL
        set <- function(y) {
                            x <<- y
                            s <<- NULL
                            }
        get <- function() x
        setinv <- function(inverse) s <<- inverse
        getinv <- function() s
        list(
                set = set, 
                get = get,
                setinv = setinv,
                getinv = getinv
            )
}


# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix. If the inverse has already been calculated (and the 
# matrix has not changed), then the cachesolve should retrieve the inverse from 
# the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
        m <- x$getinv()
        if(!is.null(m)) {
                        return(m)
                        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

