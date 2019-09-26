## A script to highlight the importance of caching
## This script is focusing on caching matrix and inverse matrix

## This function makeCacheMatrix() provides all methods (setter/getter)
## and variables to cache some matrix input and the inverse matrix
## Note: as this function returns a list, it is possible to call:
## 1/ the constructor to initialize the caching object
## Ex:      myMatrix <- makeCacheMatrix(matrix(c(6,1,5,5),2,2))
##
## 2/ the setter/getter to initialize a new cacheMatrix (if needed)
## Ex:      myMatrix$set(matrix(c(6,1,5,5,0,1,2,3,2),3,3))
##          myMatrix$get()
##
## 3/ the getter to retrieve the inversed matrix (if already calculated)
## Ex:      myMatrix$getinverse()
##
## Use cacheSolve(myMatrix) to cache the input matrix
## and calculate the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## This function cacheSolve() calculates the inverse matrix
## (or give the cache result if already calculated)
## Note: the input matrix x must be a square matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    ## Check if data is a square matrix 
    if (dim(data)[1] != dim(data)[2]) {
        print("Input matrix is not square (cannot inverse)! [STOPPED]")
        return (-1)
    } 
    
    print("Good! A square matrix to be inversed, and result is:")
    
    # create a identity matrix with the same dimensions as data
    dimMatrix <- dim(data)[1]
    identityMatrix <- matrix(0, dimMatrix, dimMatrix)
    diag(identityMatrix) <- 1
    
    # calculate the inverse matrix, assuming that: A * A(-1) = I
    # Here, data is "A" and identityMatrix is "I"
    inv <- solve(data, identityMatrix)
    # store result in cache
    x$setinverse(inv)
    # return inverse matrix!
    inv
    
}