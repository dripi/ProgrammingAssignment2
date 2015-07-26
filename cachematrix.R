## The function makeCacheMatrix contains the get/set methods used 
## for saving objects on local memory
## The function cacheSolve just computes inverse if possible

## Receive squared matrix and save it on local memory

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Recieve a special vector which contains a set of matrix (the matrix and its inverse)
## Returns teh inverse if possible

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  
  if(length(data[1,] == length(data[,1]))){
    i <- solve(data, ...)
    x$setinverse(i)
    i  
  }
  else message("not squared matrix")
  
}
