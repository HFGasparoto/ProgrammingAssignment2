## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(ma = matrix()) {
        inv <- NULL
        set <- function(y) {
                ma <<- y
                inv <<- NULL
        }
        get <- function() ma
        setinv <- function(i) inv <<- i
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(ma, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- ma$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- ma$get()
        inv <- solve(data, ...)
        ma$setinv(inv)
        inv
}
