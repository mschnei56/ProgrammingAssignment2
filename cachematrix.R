## The purpose of these functions is to store the inverse of a matrix to the
## cache, so that it can be accessed again quickly


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
                        inver <- NULL
                        set <- function(y) {
                                x <<- y
                                inver <<- NULL
                        }
                        get <- function() x
                        setinver <- function(inverse) inver <<- inverse
                        getinver <- function() inver
                        list(set = set, get = get,
                        setinver = setinver, getinver = getinver)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getinver()
        if(!is.null(inver)) {
                message("getting cached inverse")
                return(inver)
        }
        data <- x$get()
        inver <- solve(data, ...)
        x$setinver(inver)
        inver
}
