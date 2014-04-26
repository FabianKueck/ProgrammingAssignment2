## This file defines two functions: makeCacheMatrix() and cacheSolve(). 
## These functions enables the user to calculate the inverse of a square matrix.
## Furthermore if the inverse matrix of a given matrix has already been calculated, 
## it will not be calculated again, but the calculated value will be returned.
## This shall save computation time .



## This function creates a special matrix that stores the original matrix
## and the inverse equivalent. This special matrix can be used with the function 
## cacheSolve().
## Parameter x is the original matrix, that shall be inversed within cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
    ## create and initialize cachedInverse variable, that will be used to store inverse matrix
    cachedInverse <- NULL
    ## create function to set original matrix (and reinitialize cachedInverse)
    setOriginalMatrix <- function(y){
        x <<- y
        cachedInverse <<- NULL
    }
    ## create function to get original data
    getOriginalMatrix <- function() x
    ## create function to save inverse matrix in cachedInverse
    setInverseMatrix <- function(inverseMatrix) cachedInverse <<- inverseMatrix
    ## create function to get saved inverse matrix
    getInverseMatrix <- function() cachedInverse
    ## return list with the created functions
    list(setOriginalMatrix = setOriginalMatrix, getOriginalMatrix = getOriginalMatrix,
         setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


## This function returns the inverse matrix of the data saved in x
## If the inverse matrix has already been calculated in an earlier call to this function
## then the cached matrix is returned. If it has not yet been calculated, it is calculated 
## and cached in x.
cacheSolve <- function(x, ...) {
    ## Check if the inverse matrix has already been calculated
    inverse <- x$getInverseMatrix()
    if (!is.null(inverse)){
        ## The inverse matrix has already been calculated, hence we can directly return it
        message("getting cached data")
        return (inverse)
    }
    message("calculate inverse data")
    ## The inverse matrix has not been calculated yet
    ## Get the original Matrix
    data <- x$getOriginalMatrix()
    ## And calculate the inverse matrix
    inverse <- solve(data, ...)
    # save the inverse matrix for later use
    x$setInverseMatrix(inverse)
    inverse
}
