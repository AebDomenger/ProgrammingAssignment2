## 1st function creates the storage system
## 2nd function interrogates the storage system and completes it if needed

## Creates a matrix used to store in cache the inverted matrix of x once calculated

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  cached_matrix<-list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
       return(cached_matrix)
}


## Check for the existence of x^(-1) in the cache and returns it if present, calculates it and stores in if not

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- cached_matrix$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- cached_matrix$get()
  s <- solve(data, ...)
  cached_matrix$setsolve(s)
  s
}
