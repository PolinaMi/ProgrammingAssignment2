## Pair of functions to create object capable of storing matrix and its inverse,
## and to cache computation of the inverse

## Make object "matrix" capable of storing its own inverse

makeCacheMatrix <- function(x = matrix()) {
        x_inv <- NULL # this value indicates that inverse(x) wasn't calculated
        setmatrix <- function (y) {
                x <<- y # changes value of passed argument x to matrix y
                x_inv <<- NULL # resets the value of inverse(x) since new x was established
        } 
        getmatrix <- function () x # return value of x
        setinverse <- function (outer_inv) x_inv <<- outer_inv # set the value of inverse(x) to be stored
        getinverse <- function () x_inv # return value of stored inverse(x)
        # returns list of functions operating on x
        list (setmatrix = setmatrix, getmatrix = getmatrix, 
              setinverse = setinverse, getinverse = getinverse)
        
}


## Calculate the inverse matrix of x if it wasn't calculated; return cached inverse otherwise

cacheSolve <- function(x, ...) {
        x_inv <- x$getinverse() # inverse matrix for x data
        # if inverse is calculated yet
        if (!is.null(x_inv)) {
                message("getting cached inverse matrix")
                return(x_inv) # just pull its value out and exit function
        }
        # if inverse for x wasn't calculated
        x_matrix <- x$getmatrix() # get initial matrix to invert
        x_inv <- solve(x_matrix) # calculate inversion of initial matrix
        x$setinverse(x_inv) # store it in x object
        ## Return a matrix that is the inverse of 'x'
        x_inv
}
