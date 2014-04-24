## These functions have the goal of caching the inverse of a Matrix
## to avoid the repetition of this calculation

## This function receveives a invertible matrix and creates a list
## with the following functions:
##   a) set: receives a new matrix and replace the original one (variable x);
##   b) get: returns the matrix;
##   c) setInverse: receives the inverse of the matrix and keeps in a variable (m);
##   d) getInverse: returns the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ##
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(Solve) m <<- Solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function receveives the list created with the function above and
## tries to recorver the its inverse using the getInverse function.
## If there is no previous inverse matrix, this function will calculate it
## and will use the setInverse function of the list to save it in the cache.
## Finally this function will return the inverse of the matrix.
cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
