## This implements a memoized (cached) version of the matrix
## type. It only caches the results of solve().
## Caveat: Extra aguments to cacheSolve() could lead to incorrect results.

## Create a special "matrix", which is really a list containing functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the cached value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL # This is the cached value.
              # NULL means it has not been caluculated.
    set <- function(y) {
        x <<- y
        i <<- NULL # Invalidate the cache value.
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Calculate the inverse of the special "matrix" created with
## makeCacheMatrix(). If the inverse has not been calculated,
## calculates the inverse and caches it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'.
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## Cache miss, so calculate the inverse and cache it.
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    ## Our return value is the inverse we just calculated.
    i
}

## Test the functions above.

testSolve <- function() {
    ## Make a nice big matrix.
    set.seed(1)
    big <- matrix(runif(1000000), 1000, 1000)
    bigCached <- makeCacheMatrix(big)
    
    cat("Calculate the inverse without caching.\n")
    speedBigUncached <- system.time(bigInvUncached <- solve(big))
    
    cat("Calcualte the inverse with the cached object.\n")
    speedBigCached1 <- system.time(bigInvCached1 <- cacheSolve(bigCached))
    
    cat("Calculate the inverse again. The cache should be populated.\n")
    speedBigCached2 <- system.time(bigInvCached2 <- cacheSolve(bigCached))
    
    cat("Now check that answers match.\n")
    stopifnot(identical(bigInvUncached, bigInvCached1))
    stopifnot(identical(bigInvUncached, bigInvCached2))
    
    cat("The first two solves should be about the same duration.\n")
    cat(sprintf("\tuncached: %.3f, cached1: %.3f\n",
                speedBigUncached[1], speedBigCached1[1]))
    stopifnot(round(speedBigUncached[1], 1) == round(speedBigCached1[1], 1))
    
    cat("The second cached result should be substantially faster.\n")
    cat(sprintf("\tcached1: %.3f, cached2: %.3f\n",
                speedBigCached1[1], speedBigCached2[2]))
    stopifnot(speedBigCached1[1] / speedBigCached2[1] > 20)
}
