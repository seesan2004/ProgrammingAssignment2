#Function "makeCacheMatrix" creates a special "matrix" object that 
#can cache its inverse. 
#makeCacheMatrix contains 4 functions: set, get, setinverse, getinverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}  

#Function "cacheSolve" computes the inverse of the special "matrix" 
# returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

# How to run these matrix functions
# CachedMarix <- makeCacheMatrix(matrix)
# cacheSolve(CachedMarix)

#create an example Matrix
testMatrix <- matrix (1:4,2,2)
#call makeCacheMatrix() using testMatrix as in argument and store this in a CachedMarix

CachedMarix <- makeCacheMatrix(testMatrix)
# Call cacheSolve(CachedMarix) using CachedMarix as in argument
cacheSolve(CachedMarix)
