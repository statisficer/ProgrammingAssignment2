## Matrix inversion is an expensive operation and caching results improves
## performance

# makeCAcheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of matrix
# 3. set the inverse value of the matrix
# 4. get the inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y) {
    x <<- y
    m <<- NULL
}
get <- function()x
setInverse <- function(inverse) m <<- inverse
getInverse <- function() m
list(set = set, get = get,
     setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve returns the inverse of the matrix using solve()
# it first checks if the result is available in cache
# if the result is in cache, it is returned from the cache
# if inverse is not in cache, it computes the inverse, caches it and returns the result
#

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if (!is.null(m)) {
        message("getting cached matrix inverse")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m
}
