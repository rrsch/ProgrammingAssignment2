## Coursera R Programming Course 
## Programming Assignment 2: Lexical Scoping 
##


## makeCacheMatrix: create the cache matrix
## populate with functions used later.
##
## usage: 
## > m<-matrix(2:9,2,2)
## > mc<-makeCacheMatrix(m)
##
## 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}


## cacheSolve: cached inverter function
##
## 1) Checks if inverse already exists.  If so return it.
## 2) If not, solve, store, and return.
##
## Usage:
## 
## > m<-matrix(2:9,2,2)
## > mc<-makeCacheMatrix(m)
## > cacheSolve(mc)
## [,1] [,2]
## [1,] -2.5    2
## [2,]  1.5   -1
## > cacheSolve(mc)
## getting cached data
## [,1] [,2]
## [1,] -2.5    2
## [2,]  1.5   -1
## 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
