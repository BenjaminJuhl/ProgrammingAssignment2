## This is a submission to the ProgrammingAssinment2 due in week 3 of the
## "R Programming" module of the Data Science Specialization
## Two functions are contained: makeCacheMatrix, which creates a special matrix
## object, that can also store the inverse of the matrix, and cacheSolve, which
## will calculate the inverse of a matrix and store it in the cache, or use the
## cached inverse if it exists.
##
## Example:
## 1. Initiate a cached matrix object
##      cachedMatrix <- makeCacheMatrix()
## 2. Set a value for the cached matrix, here a 2x2 matrix
##      cachedMatrix$set(matrix(c(4,3,3,2),2,2))
## 3. Use cacheSolve to compute to inverse of the matrix
##      cacheSolve(cachedMatrix)
## Repeated calls of cacheSolve(cachedMatrix) will draw the returned result from
## the cache and not compute it. Changing the value of the cached matrix will
## remove the cached inverse matrix.

## 1.makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL ## if the value of the matrix is changed,
                                 ## the stored inverse is reset.
        }
        get <- function() x
        setinverse <- function(solved) inverse <<- solved
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}



## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        } else {
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        return(inverse)
        }
}
