## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
        #Computing the inverse of a square matrix can be done with the solve function in R. 
        
        #For example, if X is a square invertible matrix, then solve(X) returns its inverse.
        #For this assignment, assume that the matrix supplied is always invertible.
        
        #check if the matrix x is solveable, if not throw an error
        #out <- tryCatch(solve(x) %*% x, error = function(e) e)
        
                #clear the matrix
                m <- NULL
                set <- function(y) {
                        #save whatever is being saved in the set function to x
                        x <<- y
                        #reset the matrix to NULL
                        m <<- NULL
                }
                get <- function() {
                        #return whatever is in the main function
                        x
                }
        
                setinverse  <- function(inverse) {
                        #saves what is stored as the inverse into m
                        m <<- inverse
                }
        
                getinverse <- function() {
                        #return what is in the m variable
                        m
                }
                
                #Declare the functions and return them as a list
                #When makeCacheMatrix is called, any object can have these functions applied
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
        
        #if there is an error trying to solve the matrix return error message
        #any(class(out) == "Error, cannot solve the matrix", x)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
        #If the inverse has already been calculated (and the matrix has not changed), 
        #then the cachesolve should retrieve the inverse from the cache.
        
        #retrieve the inverse if it exists
        #subset the main function using "$" + main function 
        m <- x$getinverse()
        
        #The first thing cachemean does is to verify the value m, stored
        #previously with getmean, exists and is not NULL. 
        #If it exists in memory, it simply returns a message and the
        #value m, that is supposed to be the mean, but not necessarily.
        
        #if the matrix is empty in the current environment (or not set to NULL), 
        #get the cached inverse
        if(!is.null(m)) {
                message("getting cached inversed data")
                return(m)
        }
        
        #get the data stored in makeCacheMatrix
        data <- x$get()
        
        #calculate the inverse and save to m
        m <- solve(data, ...)
        
        #save the cached inverse of m into the object assigned in makeCacheMatrix
        x$setinverse(m)
        m

}
