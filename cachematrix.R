## Caching the Inverse of a Square Invertible Matrix
## A pair of funtions that compute and cache the inverse of a matrix

## The first function makeCacheMatrix creates a special matrix
## It is used to set and get the value of the matrix and
## Also set and get the value of the matrix inverse
## e.g. x <- makeCacheMatrix(matrix(c(7, 3, -2, 5), 2:2))

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) i <<- solve
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## The second function cacheSolve computes the inverse of the special matric
## by using the solve function
## When the cacheSolve function is run the first time, it computes the inverse
## of the special matrix and sets the retured value (i) in the cache.
## Subsequently, when the cacheSolve function is run, it retrieves the inverse (i)
## from the cache thereby saving time and avoiding recomputation.
## e.g. x <- makeCacheMatrix(matrix(c(7, 3, -2, 5), 2:2))
## cacheSolve(x)

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}