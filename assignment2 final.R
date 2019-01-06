makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
             x <<- y
             inv <<- NULL
         }
       get <- function() x
       setinverse <- function(inverse) inv <<- inverse
       getinverse <- function() inv
       list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
#EXPLANATORY COMMENT :THE ABOVE VECTOR BASICALLY CREATES A MATRIX FOR INVERSION BY CACHING

cacheSolve <- function(x, ...) {
       inv <- x$getinverse()
       if(!is.null(inv)) {
            message("getting cached data.")
             return(inv)
         }
      data <- x$get()
       inv <- solve(data)
       x$setinverse(inv)
       inv
   }	
#EXPLANATORY COMMENT :THE ABOVE VECTOR WILL FIND THE INVERSE OF MATRIX GIVEN BY 'MakeCacheMatrix'