## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
        #Computing the inverse of a square matrix can be done with the solve function in R. 
        
        #For example, if X is a square invertible matrix, then solve(X) returns its inverse.
        #For this assignment, assume that the matrix supplied is always invertible.
        
        #check if the matrix x is solveable, if not throw and error
        out <- tryCatch(solve(x) %*% x, error = function(e) e)
                
                inverse = solve(x)
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setmatrix <- function(makeCacheMatrix) m <<- cacheSolve
                getmatrix <- function() m
                list(set = set, get = get,
                     setmatrix = setmatrix,
                     getmatrix = getmatrix)
        
        any(class(out) == "Error, cannot solve the matrix", x)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
        #If the inverse has already been calculated (and the matrix has not changed), 
        #then the cachesolve should retrieve the inverse from the cache.
        m <- x$getmatrix()
        
        #The first thing cachemean does is to verify the value m, stored
        #previously with getmean, exists and is not NULL. 
        #If it exists in memory, it simply returns a message and the
        #value m, that is supposed to be the mean, but not necessarily.
        
        if(!is.null(m)) {
                message("getting cached inversed data")
                return(m)
        }
        data <- x$getmatrix()
        m <- cacheSolve(data, ...)
        x$setmatrix(m)
        m

}
