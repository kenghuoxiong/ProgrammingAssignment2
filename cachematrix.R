## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##creates a special Matrix like the example and save the data of the matrix and value of its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  print(get)
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
##if already calculate the inverse and have the cache then skip the computation. Otherwise it will calculate the
##inverse of the matrix
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m      ## Return a matrix that is the inverse of 'x'
}
