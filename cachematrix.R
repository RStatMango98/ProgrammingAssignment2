## Assignment 2: Lexical Scoping
## Caching the Inverse of a Matrix: 
## Computing the inversion of a matrix being costly and time-consuming, 
## caching the inverse is advantageous rather than computing it repeatedly.

## A pair of functions, to create an object which stores the matrix - specific
## matrix - and cache its inverse, is mentioned below. The following is the
## function that creates a specific object of matrix.

makeCacheMatrix <- function(x = matrix(10:20, 2, 2)) 
   {
   inv <- NULL
   set <- function(y) 
     {
     x <<- y
     inv <<- NULL
     }
   get <- function() x
   setInverse <- function(inverse) inv <<- inverse
   getInverse <- function() inv
   list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)
   }

## Computation of inverse of the matrix is performed by this function, which is
## created by makeCacheMatrix, mentioned above.

## Pre-calculated inverse (of a specific, unchanged matrix), if any, should
## retrieve the inverse from the stored information (cache).

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   inv <- x$getInverse()
   if (!is.null(inv)) 
   {
   message("Getting cached data...")
   return(inv)
   }
   mat <- x$get()
   inv <- solve(mat, ...)
   x$setInverse(inv)
   inv
   }

##Proceed.
