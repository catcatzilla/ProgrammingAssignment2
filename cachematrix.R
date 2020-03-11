## The makeCacheMatrix return the list of 4 functions. 
##It will put the matrix and the calculated inverse matrix in cache.

makeCacheMatrix <- function(x = matrix()) {
  m <- matrix()
  set <- function(y = matrix()){
  x <<- y
  m <<- matrix()
  }
  get <- function() x 
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

##This function will first check if the inverse matrix m in cache is NA. 
##If it is NA, it will calculate and return the m. if it is not, it will take from cache.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(all(!is.na(m))) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

