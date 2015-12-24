##
# Functions defined in this file creates a special object that caches the matrix 
# and its inverse form. Whenever the inverse of the matrix is computed,
# the computation logic checks for an existing inverse and if found,
# returns the cached inverse.  If invese is not found, then the inverse is computed 
# which gets cached so that the next time the inverse is called, the cached copy 
# will be returned.
##
## Usage:
#   specialMatrix <- makeCacheMatrix(invertibleMatrix)
#   cacheSolve(specialMatrix)
#   cacheSolve(specialMatrix)
#   cacheSolve(specialMatrix)
#   cacheSolve(specialMatrix)
#Note: On the first call to cacheSolve(...), the inverse of the matrix is called.
# On all the subsequest calls to cacheSolve(...), the cached copy of the inverse
# is returned.
##

## makeCacheMatrix is a function that returns a special R Object that supports
# four methods.
# 1. setMatrix :  This method can be used to either set or change the matrix.
#                 When this method is called, the inverse of the matrix is set to NULL.
# 2. getMatrix :  Returns the current matrix for which the inverse needs to be calculated.
# 3. setInverse : This method is used to set the inverse of the matrix that was set using setMatrix.
# 4. getInverse : Returns the current inverse matrix tha was set using for setInverse or return NULL.

makeCacheMatrix <- function(x = matrix()) {
    xInvCached <- NULL
    
    setMatrix <- function(mat) {
        x <<- mat
        xInvCached <<- NULL
    }
    
    getMatrix <- function() x
    
    setInverse <- function(xInverse) xInvCached <<- xInverse
    
    getInverse <- function() xInvCached
    
    list(setMatrix=setMatrix, getMatrix=getMatrix, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve function takes a special R object that is created using the above function
# makeCacheMatrix().  CacheSolve first checks if the inverse of the matrix already exists
# by calling x$getInverse(). 
#       If the value of x$getInverse() is not NULL,
#           then the value is returned
#       If the value of x$getInverse() is NULL, 
#           then inverse of the matrix is computed, and stored in x (special R object) 
#           and then the inverse is returned.
# Subsequent calls to cacheSolve on the same object x will not result in computing the inverse
# unless x$setMatrix() is called.
##
# Note: There is an assumption that the matrix supplied is always invertible.
##

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    # get the cached value of the inverse matrix
    inverseOfX <- x$getInverse()
    
    # if the cached value is not null, simply return it.
    if(!is.null(inverseOfX)) {
        return(inverseOfX)
    }
    
    # else, caclulate the inverse and store it using setInverse()
    # the cache
    xMatrix <- x$getMatrix()
    inverseOfX <- solve(xMatrix)
    x$setInverse(inverseOfX)
    
    # return the inverse of x
    inverseOfX
}

