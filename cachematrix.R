## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix that will have its
# inverse cached. If his matrix has its inverse given to it with the
# setInverse function, it  will remember this value.
# If the value of this matrix is changed
# with the set function, then it will remove the remembered value of the inverse.
# This matrix will also return a null value if it does not know the value of
# its inverse.

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
         x <<- y
         m <<- NULL
     }
     get <- function() x
     setInverse <- function(inverse) m <<- inverse
     getInverse <- function() m
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function will return the inverse of the special matrix that is passed
# to it. This function will first ask the matrix for its inverse.
# If the value returned is not null, then is function returns this value as the inverse.
# If the value passed to it from the special matrix is null, then this 
# function will calculate the inverse and then store this answer inside
# the special matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    matrix <- x$get()
    m <- solve(matrix)
    x$setInverse(m)
    m
}
