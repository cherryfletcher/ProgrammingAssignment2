## The following functions cache the inverse of a matrix

## makeCacheMatrix creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  s<- NULL
  set<- function(y) {
    x<<- y
    s<<- NULL
  }
get<- function() x
setSolve<- function(solve) s<<-solve
getSolve<- function() s
list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## cacheSolve computes the inverse of the special matrix. 
## If the inverse has already been calculated, the inverse is retrieved from the cache

cacheSolve <- function(x, ...) {
  s<- x$getSolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data<- x$get()
  s<- solve(data, ...)
  x$setSolve(s)
  s
}

