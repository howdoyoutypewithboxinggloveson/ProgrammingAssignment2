## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
## Your assignment is to write a pair of functions that cache the inverse of a matrix.

## Write the following functions:
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##    If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##    Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

## For this assignment, assume that the matrix supplied is always invertible

## Put comments here that give an overall description of what your
## functions do

##Caches the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  
  ## initializing local variable m to NULL
  m <- NULL
  
  ## set can be called if makeCacheMatrix is called with no argument specified 
  ##    takes matrix as argument
  set <- function(y) {
        x <<- y
        m <<- NULL
  }
  
  ## returning the matrix
  get <- function() x
  
  ## setmatrix takes matrix inverse as argument, and stores in m
  ## matrix inverse supplied by cacheSolve by 
  ##    calling solve() on x stored in get() method of makeCacheMatrix
  setmatrix <- function(inverse) m <<- inverse
  
  ## returning the matrix inverse
  getmatrix <- function() m
  
  ## storing function values in list format
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

##computes the matrix inverse
##takes makeCacheMatrix as argument
cacheSolve <- function(x, ...) {
        
        ##calling matrix inverse stored in getmatrix() method of makeCacheMatrix
        m <- x$getmatrix()
        if(!is.null(m)) {
               message("getting cached data")
               return(m)
        }
        
        ##calling matrix stored in get() method of makeCacheMatrix
        data <- x$get()
        
        ##getting matrix inverse of matrix called from makeCacheMatrix
        m <- solve(data, ...)
        
        ##storing matrix inverse in setmatrix() method of makeCacheMatrix
        x$setmatrix(m)
        m
}
