# Coursera - R Programming Assignment 2
# Assignment:   Caching the Inverse of a Matrix
# Date:         4/26/2014

# This function creates a customized "matrix" object  
# that can cache its inverse. When a new value is assigned
# to this object, the cache will be cleared.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# This function computes the inverse of the special 
# "matrix" returned by makeCacheMatrix above. If the 
# inverse has already been calculated (and the matrix 
# has not changed), then cacheSolve should retrieve 
# the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  message("no cached data. compute inversion")
  return(inv)
}


# Test case
mat1 <- matrix(c(1,0,0,1),2)
mat2 <- matrix(c(1,2,3,4),2)
mat3 <- matrix(c(126,301,762,510),2)
for (mat in list(mat1, mat2, mat3)){
  message("Test case")
  a <- makeCacheMatrix()
  a$set(mat)
  # first time - no cache
  print(cacheSolve(a)) 
  # second time - access cached data
  print(cacheSolve(a)) 
  # compare with the build-in solve() result
  message("compare with the build-in solve() result")
  print(cacheSolve(a) == solve(mat)) 
}




