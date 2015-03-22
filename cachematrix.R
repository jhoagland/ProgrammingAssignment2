
## Creates a matrix with a list of several functions including setting the matrix, getting the matrix, setting the inverse and getting the inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  sinverse <- function(inverse) inver <<- inverse
  ginverse <- function() inver
  list(set=set, get=get, setinverse=sinverse, getinverse=ginverse)
}


## Returns a matrix that is the inverse of x

cacheSolve <- function(x, ...) {
  i<- x$getinv()
  if(!is.null(i)) {
    message("getting cache")
    return(i)
  }
  data <- x$get()
  i<- solve(data, ...)
  x$setinv(i)
  i
}
