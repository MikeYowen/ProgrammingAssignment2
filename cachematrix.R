## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) { ## Sets the function to matrix x and i to NULL
                x <<- y
                i <<- NULL
        }

        get <- function() x ## Returns x
        setInverse <- function(inverse) i <<- solve(x) ## Sets the inverse of x to i
        getInverse <- function() i ## Returns the inverse of x (i)
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        i <- x$getInverse() ## Gets current inverse from cache
        if(!is.null(i)) { ## Checks if inverse is blank
                message("getting cached data")
                return(i)
        } else { ## If 'i' is blank (empty)
                data <- x$get() ##assign matrix to 'data'
                i <- solve(data, ...) ## inverse 'data'
                x$setInverse(i) ## update cache with new inverse
                i ## print inverse
        }
}
