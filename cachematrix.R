## The makeCahceMatrix function builds a set of functions
## and returns them in a list. The 4 functions are 
## set(), get(), getinverse(), and setinverse(). 
## It also contains 2 data objects, x & m. m is set to NULL
## whenever the function is called. makeCahceMatrix
## will later be called by the cacheSolve function. The 
## pointers to the parent environment results in the 
## function not being removed from memory.

## The function creates an object of type makeCacheMatrix()
## when the function finishes running. This object will later 
## be called by the cacheSolve function. 

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


## cacheSolve makes calls to the makeCacheMatrix object.
## It gets the inverse matrix if m is NULL, otherwise it 
## pulls the inverse matrix from cache and displays it to the user.
## A message of getting cached data is displayed if the cahceSolve
## is run mulitple times for the same matrix. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
