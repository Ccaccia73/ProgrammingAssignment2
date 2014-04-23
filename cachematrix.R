## EXAMPLE usage:
# m<-matrix(data=rnorm(25,0,1),nrow=5)
# mc<-makeCacheMatrix(m)
# invm<-cacheSolve(mc)
#
# the following should give a 5x5 identity matrix (with reasonable approx)
# m %*% invm 



## makeCacheMatrix generates an object with 4 methods (functions) to
#  set and get the original matrix and its inverse. It returns a list
#  with the 4 functions as elements


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(computedinv) inv <<- computedinv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve computes the inverse of a matrix using the the list
#  of functions proded by makeCacheMatrix.
#  if the inverse has already been computed it returns it
#  otherwise it computes the inverse, caches it in the object and returns it



cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
