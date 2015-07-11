## Put comments here that give an overall description of what your
## functions do

# Creates a special "matrix", which is really a list containing a
# function to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# The following function calculates the inverse of the special
# "matrix" created with the above function. However, it first
# checks to see if the inverse has already been calculated. If
# so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the data and sets the value
# of the inverse in the cache via the setinv function.
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
