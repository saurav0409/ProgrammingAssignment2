
makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) a <<- inverse
  getinv <- function() a
  list(set = set,
       get = get,
       setinv = setinv,
       getinv= getinv)
}
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  a <- x$getinv()
  if (!is.null(a)) {
    message("got cached")
    return(a)
  }
  data <- x$get()
  a <- solve(data, ...)
  x$setinv(a)
  a
}
