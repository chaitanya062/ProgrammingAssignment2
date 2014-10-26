#Accepts matrix as input and returns functions set,get,set input and get input
makeCacheMatrix <- function(x = matrix())
  {
  
     m <- NULL
     set <- function(y) {
                           x <<- y
                           m <<- NULL
                        }
     get <- function() x
     setInverse <- function(Inverse) m <<- Inverse
     getInverse <- function() 
       
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
	 }
#Calls makeCachematrix to compute inverse of the matrix 
cacheSolve <- function(x, ...) 
  {
  
  i <- x$getInverse()
  if(!is.null(i)) {
           
       message("getting cached data")
       return(i)
          }
  data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}



