makeCacheMatrix <- function(x = matrix()) {
  # http://stackoverflow.com/questions/24904683/caching-the-mean-of-a-vector-in-r
  # https://www.snip2code.com/Snippet/803831/R-course-ProjectAssigment2
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  # setinverse <- function(solve) m <<- inverse
  setinverse <- function(inverse) inv <<- inverse
  #getmatrix <- function() inv
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  
  # https://www.snip2code.com/Snippet/803831/R-course-ProjectAssigment2
  
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  matrix_inv <- x$get()
  inv <- solve(matrix_inv, ...)
  x$setinverse(inv)
  inv
}
