## Cashing Matrix Inversion. Module contains two functions. 
## 1. for creating a matrix object that can cash its inverse.
## 2. A function to compute inverse & use cashed version if there 
##    if there is one instead of re-creating inverse.

## function that creates & manages inverse.
## retuens list of utility functions.

## example: 
## source("cachematrix.R")
## a <- makeCasheMatrix(matrix(rnorm(1:16),4,4))
## cacheSolve(a)
## second time onwards cacheSolve(a), will return cached inv.

makeCacheMatrix <- function(x = matrix()) {

   inv <- NULL         
   set <- function(m) {       ## saves matrix & initializes invese  
         x <<- m
         inv <<- NULL
   }
   
   get <- function() x          ## returns matrix
   
   setinv <- function(i)  inv <<- i
   getinv <- function() inv
   
   ## return of makeCacheMatrix() is a list of functions
   
   list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## function retrieves the inverse it is cashed or computes
## inveese if not in cash

## x is of type list, returned by makeCacheMatrix()

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if ( !is.null(inv)) {
          message("Returning Cached Matrix")
          return(inv)
    }
     
    m <- x$get()      ## get the matrix 
    
    inv <- solve(m)   ## find inverse
    
    x$setinv(inv)     ## saves the inverse
    
    inv               ## return inverse
    
}
