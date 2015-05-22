# Coursera and Johns Hopkins University
# Data Science Specialization
# R Programming - Assignment 2
# Author: Adam J. Gardner
# May 2015

# makeCacheMatrix is the function that contains a subset of
# functions for caching matrices.
makeCacheMatrix <- function(original_matrix=matrix()) {
    inverse_matrix <- NULL

    # Declare a function to store the original matrix.
    setmatrix <- function(y) {
        original_matrix <<- y
        inverse_matrix <<- NULL
    }
    
    # Declare a function to return the original matrix.
    getmatrix <- function() original_matrix
    
    # Declare a function to store the inverse matrix.
    setinverse <- function(inverse) inverse_matrix <<- inverse
    
    # Declare a function to return the inverse matrix.
    getinverse <- function() inverse_matrix
    
    # Store the declared functions in a list so that when
    # makeCacheMatrix is assigned to an object, it contains all
    # four functions.
    list(setmatrix = setmatrix, getmatrix = getmatrix,
         setinverse = setinverse, getinverse = getinverse)
}

# cacheSolve is the function that either pulls the previously-
# cached matrix, or computes the matrix and caches it for next
# time.
cacheSolve <- function(original_matrix=matrix()) {
    # Declare makeCacheMatrix() for use within cacheSolve
    cachefunction <- makeCacheMatrix()
    
    # Get and store the previously-cached inverse matrix.
    inverse_matrix <- cachefunction$getinverse()

    # Confirm that the matrix returned exists. If so, return
    # the previously-computed inverse matrix and stop the
    # function.
    if(!is.null(inverse_matrix)) {
        return(inverse_matrix)
    } # Everything below this line will only be read if a
      # previously-cached inverse matrix is not found.
    
    # Invert and store the matrix.
    inverse_matrix <- solve(original_matrix)
    
    # Cache the matrix that was passed to the function.
    cachefunction$setmatrix(original_matrix)
    
    # Cache the newly-inverted matrix.
    cachefunction$setinverse(inverse_matrix)
    
    # Return the newly-computed inverted matrix.
    inverse_matrix
}