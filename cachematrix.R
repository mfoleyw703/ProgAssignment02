## Michael Foley- Programming Assignment 2: Lexical Scoping
## Matrix inversion is usually a costly computation and there may be some benefit to caching the 
## inverse of a matrix rather than compute it repeatedly.  The following functions: 
## 1. Create a matrix object that can be cached as its inverse. 
## 2. Compute the inverse of the special matrix defined by the makeCacheMatrix function.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix<- function(m = matrix()){
  inv <- NULL
  set <- function(y){
    m<<- y
    inv <<- NULL
  }
  get <- function() m
  setInverse <- function(Inverse) inv<<- Inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function

cacheSolve<-function(m,...){
  inv <- m$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  thematrix <- m$get()
  inv <- solve(thematrix,...)
  m$setInverse(inv)
  inv
}
