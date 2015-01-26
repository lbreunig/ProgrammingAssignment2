#The purpose of the following 2 functions is to cache the inverse of a matrix.

#makeCacheMatrix function creates a special "matrix" object that can cache its inverse
#The function set value of the matrix
#Get the value of the matrix
#Set The value of the solve
#Get the value of the solve

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setmean <- function(solve) s <<- solve
  getmean <- function() s
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

#cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix function. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getmean()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setmean(s)
  s
}

