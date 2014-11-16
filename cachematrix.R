## Programming Assignment 2
##  calculate the inverse of a matrix and store it
##  as an optimization

##  this is a two part solution
##  the first part of the solution implements a matrix cache
##  which is defined in makeCacheMatrix

##  the second part of the solution first checks to see if a
##  cached matrix inverse exists, if it does, return this
##  if not, calculate the inverse, cache it, and return the inverse

## makeCacheMatrix
##  implements a cache for an inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(mtx) m <<- mtx
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)  
}


## cacheSolve
##  this function calculates the inverse or solution to a matrix
##  it first checks to see if the inverse has already been calculated
##  if so, return that
##  if not, calculate and cache the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ##get the matrix pointed to by X
  ##if a matrix there exists, return it
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  
  ##no matrix exists, so compute the inverse, cache it
  ##then return it
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m  
}

