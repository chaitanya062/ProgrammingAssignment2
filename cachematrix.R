## Assignment2
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i<<- inverse
  getinverse <- function() i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data.")
    return(i)
  }
  data <- x$get()
  i<- solve(data)
  x$setinverse(inv)
  i
}


x=rbind(c(2,1),c(1,4))
m=makeCacheMatrix(x)
m$get()
