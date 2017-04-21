#Matrix inversion is usually a costly computation and there may be some benefit
#to caching the inverse of a matrix rather than compute it repeatedly.
#
#This file has two functions that will be used to cache the inverse of a square
#matrix. As per the instructions in the assignment, it is assumed that the 
#matrix used as input to thefunctions is always invertible!

## 
#This first function, "makeCacheMatrix" creates a special "vector", which is 
#actually is a list of functions that will be used to perform the below actions:
#
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse of a matrix
#4. get the value of the inverse of a matrix



makeCacheMatrix <- function(x = matrix()) {
        My_Inverse <- NULL
        setMatrix <- function(y) {
                x <<- y
                My_Inverse <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(New_Inverse) My_Inverse <<- New_Inverse
        getInverse <- function() My_Inverse
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}

##
# This second function, "cacheSolve" calculates the inverse of the special 
#"matrix object" created with the above function. However, it first checks 
#to see if the inverse of the matrix passed has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the matrix and sets the inverse
# of the matrix passed in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		        Inverse <- x$getInverse()
        if(!is.null(Inverse)) {
                message("getting cached data")
                return(Inverse)
        }
        data <- x$getMatrix()
        Inverse <- solve(data, ...)
        x$setInverse(Inverse)
        Inverse
}