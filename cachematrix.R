# Matrix inversion is usually a costly computation and their may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly. 
# The following functions caches the inverse of a matrix.

# This function creates a special "matrix" object that can cache its inverse.
# The special "matrix" is a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL # if the matrix change, the inverse must be recalculated
        }
        get <- function() x
        setinverse <- function(inverse) inver <<- inverse
        getinverse <- function() inver
        
        # Return the "special" matrix
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        inver <- x$getinverse()
        if(!is.null(inver)) { # if was computed previously
                message("Getting cached data.")
                return(inver)
        }
        data <- x$get()
        inver <- solve(data) # solve function gets the inverse of a matrix
        x$setinverse(inver)
        
        ## Return a matrix that is the inverse of 'x'
        inver
}
