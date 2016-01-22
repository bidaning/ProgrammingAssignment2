## Caching the Inverse of a Matrix

## The first function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix(0)) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    } #set original matrix
    get <- function() x #get original matrix
    setinv <- function(inverse) inv <<- inverse #set the inverse of the matrix
    getinv <- function() inv #get the inverse of the matrix
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## The second function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse matrix") #the inverse has been computed before
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...) #calculate inverse of the matrix
    x$setinv(inv)
    inv #the inverse matrix is newly computed if no message showing up
}

## Following is an example for test

# w<- matrix(c(1,3,4,6),2,2)
# x<- makeCacheMatrix(w)
# x$get()
# cacheSolve(x)
# solve(w)

