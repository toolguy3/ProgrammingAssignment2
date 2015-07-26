## Author: Andre Jacques
## Course: rprog-030
## Assignment 2
## The following function enable the user to calculate and cache the inverse of a square matrix.
## The goal is the save time by not repeating a time-consuming calculation.


## makeCacheMatrix is a function which creates a special vector
## the special vector is really a list of 4 functions:
##    "set": set the value of the matrix
##    "get": get the value of the matrix
##    "setinv": set the value of the inverse matrix
##    "getinv": get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        invMat <- NULL
        set <- function(y) {
                x <<- y
                invMat <<- NULL
        }
        get <- function() x
        setinv <- function(solve) invMat <<- solve
        getinv <- function() invMat
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve is a function which calculates the Inverse of the matrix
## If the inverse has already been calculated, it just returns the value found using "getinv"
## If not: 
##       it calculates the inverse matrix using the "solve" function
##       it stores the inverse matrix in the cache using "setinv"
##       Returns the inverse matrix

cacheSolve <- function(x, ...) {


        invMat <- x$getinv()
        if(!is.null(invMat)) {
                message("getting cached data")
                return(invMat)
        }
        dataMat <- x$get()
        invMat <- solve(dataMat, ...)
        x$setinv(invMat)
        invMat


}
