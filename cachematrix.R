## functions to calculate and cash matrix inverse

## turns matrix into an object with cache slots

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsol <- function(solve) m <<- solve
  getsol <- function() m
  list(set = set, get = get, setsol = setsol, getsol = getsol)
}


## calculates the inverse of the cached-matrix-object or, if available,
## return the cached inverse

cacheSolve <- function(x, ...) {
  m <- x$getsol()
  if(!is.null(m)) {
    print('cached data available')
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsol(m)
  m
}


