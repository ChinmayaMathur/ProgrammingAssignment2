## makeCacheMatrix is function that produces a special matrix object that can store its inverse in the cache
## here  x is the special matrix and i is the inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function()x
  setinverse <- function(solve) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is function that that computes the inverse of the special matrix. If the inverse of
## that matrix is already computed and the matrix has not been changed then it gets the inverse from 
## cache and if not then it calculates the inverse and stores the inverse of that matrix in cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}


