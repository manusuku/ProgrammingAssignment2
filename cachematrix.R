## Cashing Matrix Inversion. Module contains two functions. 
## 1. for creating a matrix object that can cash its inverse.
## 2. A function to compute inverse & use cashed version if there 
##    if there is one instead of re-creating inverse.

## How to use:
## > m <- matrix(rnorm(16), 4,4)             ##  creates matrix
## > cacheSolve(makeCacheMatrix(m))          ##  first time create inverse
## > cacheSolve(makeCacheMatrix(m))          ##  second time onwards cash is returne as below.  
## Returning Cached Matrix
## ... invesre of matrix


## function that creates & manages inverse.
## retuens list of utility functions.

makeCacheMatrix <- function(x = matrix()) {

   if (!exists("inv"))   inv <<- NULL 
           
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
