## Michiel van Zummeren
## 24-12-2015
## Programming Assingment 2, Coursera DSS

## The function makeCacheMatrix caches the 'special matrix' object, and stores this in the cache.
## The function chacheSolve looks for a matrix stored in the cache, and creates the inverse of this matrix
## The function does not check if it is possible to create the inverse matrix (n*n matrix with determinant!=0)

## I am a bit unsure about the inner workings of the function, as it a rough copy of the example provided by R. Peng
## in the course material, with some slight alterations to take the inverse instead of the mean.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {
 
   m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  inv2 <- solve(data, ...)
  x$setinv(m)
  inv2
}

## Funtions are tested with the following matrix
## [1 2
##  3 4]
## Which should solve to the inversed matrix 
## [-2 1
##  1.5 -0.5]
## To compare, the following code can be entered (after sourcing the functions)
## x = rbind(c(1, 2), c(3, 4))
## m = makeCacheMatrix(x)
## cacheSolve(m)
