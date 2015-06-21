## Functions to calculate inverse of invertible matrix and cache it.
## if inverse already computed, get from the cache rather than computing the 
## inverse again

## the below function is to cache the inverse of a matrix
## function makes a special matrix which is a list with the following:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    invM <- NULL
    set <- function(y) {
        x <<- y
        invM <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invM <<- inverse
    getinverse <- function() invM
    list(set = set, get=get, setinverse = setinverse, getinverse = getinverse)
}

## the below function is to compute the inverse of a matrix
## if already in cache, retrieve the inverse from the cache
## assuming matrix is invertible

cacheSolve <- function(x, ...) {
    invM <- x$getinverse()
    if (!is.null(invM)) {
        message ("getting cached data")
        return(invM)
    }
    data <- x$get()
    invM <- solve(data)
    x$setinverse(invM)
    invM
}
