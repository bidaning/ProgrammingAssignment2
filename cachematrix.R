## Caching the Inverse of a Matrix
makeCacheMatrix <- function(x = matrix(0)) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

# Following is an example for test

# w<- matrix(c(1,3,4,6),2,2)
# x<- makeCacheMatrix(w)
# x$get()
# cacheSolve(x)
# solve(w)

