makeCacheMatrix <- function(x = matrix()) {
  s <- NULL # sets value to NULL as default
  set <- function(y) {
    x <<- y # caches value to check if it is already in cache
    s <<- NULL # sets value to NULL as default
  }
  get <- function() x
  inverseMatrix <- function(solve) s <<- solve # finds inverse of the matrix if not already cached
  getMatrix <- function() s # this returns the matrix in the function
  list(set = set, get = get,
       inverseMatrix = inverseMatrix,
       getMatrix = getMatrix)
}
cachesolve <- function(x=matrix(), ...) {
  s <- x$getMatrix() # retrieves matrix from previous function
  if(!is.null(s)) { # checkes to see if matrix is cached
    message("getting cached data")
    return(s)
  }
  enterMatrix <- x$get() # if not cached, the matrix is entered here
  s  <- solve(enterMatrix, ...) # this solves for the inverse of the matrix
  x$inverseMatrix(s) # this caches the inverse of the matrix
  return (s)
  
