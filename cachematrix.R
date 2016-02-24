## The two functions allows to reduce the computational time in computing an inverse of a matrix. 
## Generally you should use this approach whenever you have to repeat an operation a large
## number of times (e.g. in loops).

#The first function, `makeCacheMatrix` creates a special "makeCacheMatrix", which is
# a list of four function: set, get, setsolve and getsolve.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## The second function -`cacheSolve`-, which does not contain others function inside, performs 
## a logical control to verify if the inverse of a new matrix has been already computed.
## If the statment is true than the inverse is directly obtained from the chace, where it is 
## stored, without computing again it.

cacheSolve <- function(x,...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
