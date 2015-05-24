## As required by the assignment this function creates a "matrix" object that can cache its inverse and value.
## The matrix supplied in the "x" argument should be invertible. Otherwise the function will not work.
## This function returns a list that could evaluate four functions, described below.

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        ## Function to set a new matrix for this object (it also should be invertible)
        set <- function(y)
        {
                x <<- y
                m <<- NULL
        }
        ## This function returns the current matrix associated with the object
        get <- function()
        {
                x
        }
        ## This function sets a new matrix to the object. This function is used by the "cacheSolve" function to set the invertible matrix of "x" (the original matrix).
        setinverse <- function(h)
        {
                m <<- h
        }
        ## This function returns the invertible matrix defined by the previous function. It is also used by the "cacheSolve" function to get the inverted matrix value.
        getinverse <- function()
        {
                m
        }
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##This function returns the invertible matrix of an object created by the "makeCacheMatrix" function.
##It is smart enough to check if the inverse of the matrix was already calculated. If true, it returns its value. If not, it calculates the inverse and outputs the result.

cacheSolve <- function(x, ...) {
        ##Check if the "x" object already have a calculated inverse matrix.
        m <- x$getinverse()
        
        ## If there is a value for the inverse matrix, outputs a message, the inverse matrix and exits the function.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ##If no value was found, it calculates the inverse matrix and output the result.
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
